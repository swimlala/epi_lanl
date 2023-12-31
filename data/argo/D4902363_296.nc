CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-11-01T00:35:34Z creation;2018-11-01T00:35:39Z conversion to V3.1;2019-12-19T07:29:09Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7T   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     88   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8X   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8\   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8d   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8h   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8p   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8x   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I4   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p4   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �4   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �4   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �d   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �t   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �x   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �|   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181101003534  20200115131518  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL              (A   JA  I2_0576_296                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @؍5�X^ 1   @؍6�9 @9�9Xb�d30��)1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @9��@�  @�  A��A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  BpffBx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+�fD,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @:=q@�Q�@�Q�AA (�A@(�A`(�A�{A�{A�{A�{A�{A�{A�{A�{B 
=B
=B
=B
=B 
=B(
=B0
=B8
=B@
=BH
=BP
=BX
=B`
=Bh
=Bpp�Bx
=B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�C �C�C�C�C�C
�C�C�C�C�C�C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C�HC�HC�HC�HC�C�C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC��{C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC��{C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�C�HC�HC�HC�HC�HC�HC�HC��{C��{C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HD  �D ��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D	 �D	��D
 �D
��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D  �D ��D! �D!��D" �D"��D# �D#��D$ �D$��D% �D%��D& �D&��D' �D'��D( �D(��D) �D)��D* �D*��D+ �D+�
D, �D,��D- �D-��D. �D.��D/ �D/��D0 �D0��D1 �D1��D2 �D2��D3 �D3��D4 �D4��D5 �D5��D6 �D6��D7 �D7��D8 �D8��D9 �D9��D: �D:��D; �D;��D< �D<��D= �D=��D> �D>��D? �D?��D@ �D@��DA �DA��DB �DB��DC �DC��DD �DD��DE �DE��DF �DF��DG �DG��DH �DH��DI �DI��DJ �DJ��DK �DK��DL �DL��DM �DM��DN �DN��DO �DO��DP �DP��DQ �DQ��DR �DR��DS �DS��DT �DT��DU �DU��DV �DV��DW �DW��DX �DX��DY �DY��DZ �DZ��D[ �D[��D\ �D\��D] �D]��D^ �D^��D_ �D_��D` �D`��Da �Da��Db �Db��Dc �Dc��Dd �Dd��De �De��Df �Df��Dg �Dg��Dh �Dh��Di �Di��Dj �Dj��Dk �Dk��Dl �Dl��Dm �Dm��Dn �Dn��Do �Do��Dp �Dp��Dq �Dq��Dr �Dr��Ds �Ds��Dt �Dt��Du �Du��Dv �Dv��Dw �Dw��Dx �Dx��Dy �Dy��Dz �Dz��D{ �D{��D| �D|��D} �D}��D~ �D~��D �D��D� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�C�D��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD�ÅD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD���D��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RDRD��RD� RD�@RDÀRD��RD� RD�@RDĀRD��RD� RD�@RDŀRD��RD� RD�@RDƀRD��RD� RD�@RDǀRD��RD� RD�@RDȀRD��RD� RD�@RDɀRD��RD� RD�@RDʀRD��RD� RD�@RDˀRD��RD� RD�@RD̀RD��RD� RD�@RD̀RD��RD� RD�@RD΀RD��RD� RD�@RDπRD��RD� RD�@RDЀRD��RD� RD�@RDрRD��RD� RD�@RDҀRD��RD� RD�@RDӀRD��RD� RD�@RDԀRD��RD� RD�@RDՀRD��RD� RD�@RDրRD��RD� RD�@RD׀RD��RD� RD�@RD؀RD��RD� RD�@RDـRD��RD� RD�@RDڀRD��RD� RD�@RDۀRD��RD� RD�@RD܀RD��RD� RD�@RD݀RD��RD� RD�@RDހRD��RD� RD�@RD߀RD��RD� RD�@RD��RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD��RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A˟�A˕�A�hsA�jA�hsA�ffA�dZA�bNA�bNA�bNA�^5A�bNA�^5A�ZA�ZA�ZA�ZA�ZA�VA�C�A��A��/AʶFAʑhA�`BA�1'A�hsA���A���A�bNA�VA��hA��-A�x�A��HA�A�A�33A�~�A�JA�bNA��FA��RA��RA��
A�x�A�t�A�  A��RA�l�A���A��\A��\A���A��A�t�A���A�%A���A���A�l�A��A�r�A��mA�x�A���A�bNA�33A��/A��7A�|�A�t�A�7LA��uA���A�A�ZA�1A��^A�{A���A�dZA�JA�z�A�^5A�%A�~�A�-A�A��mA���A�ffA���A���A�bA� �A�p�A�VA��A~��A}�7A{��A{Ayp�Aw��Au�As�Ar�Aq�;Ap��Ap�An�HAm�Al�!AlA�Ak;dAi��Ah��Ag�Ag�#AgƨAg�-Agx�Ae|�Ad�HAdn�Ad�Ac��Aa��A`�HA_A^~�A]��A]hsA[�;AY��AXZAV�+AS��AQ�FAP�AO�AO�ANbNAM��AM|�AM&�AL�jALffAL�AK�hAJ�AF�AE/ADA�AC33AA7LA@��A@ZA@=qA?�A?%A<ȴA;��A:�!A9�wA9�A8Q�A7�A6�yA5\)A4�9A4M�A3��A2�HA2A1t�A133A0�yA0~�A/�#A.n�A+��A*�!A*5?A)�7A)XA)�A(ȴA(-A'��A'XA&r�A$A"ĜA!�#A ��A��A�HA-A��A�A�9AM�A�A�A�A��A�A`BAS�A/A�9AS�A�A�AoA{A��AjAA�TAG�A%A�!A��A�+AVA(�A�A`BA
��A
I�AO�A��A�A�-A�yA�@�;d@�V@�b@�V@�`B@��@�V@�?}@���@�Z@�  @�v�@�@�K�@�9@���@�\@��#@��@�I�@�dZ@�J@�Z@��#@�1'@ߝ�@ޗ�@���@�1'@���@�E�@�p�@�Q�@���@�E�@Դ9@�l�@җ�@�ff@Ѻ^@�A�@��y@���@�I�@�b@���@�dZ@��@���@ư!@š�@�dZ@�V@�1@���@�I�@� �@�ƨ@�+@�-@���@���@�l�@�;d@�+@�o@�ȴ@���@�@���@���@�v�@�X@��`@��`@��`@��`@��@�Z@���@��@�x�@�&�@�%@��D@�b@�
=@�ff@�`B@�Ĝ@�A�@���@�;d@��!@�-@��R@���@�33@�@���@��`@��@���@��9@�b@���@�bN@�\)@���@���@�x�@�A�@��
@���@�\)@��@��@�X@��@��@�bN@��F@�;d@���@��!@�V@�$�@�7L@�9X@�"�@��R@�^5@�J@��#@�@��h@�p�@�O�@�7L@��@�%@���@��@���@��@�j@�I�@�9X@� �@�  @��@��;@���@��@�dZ@���@��H@��!@��\@�ff@�-@��@��T@���@�@���@�G�@���@�dZ@�33@��@�=q@��`@��`@���@�%@�%@��9@�|�@�\)@�;d@�"�@��y@�{@�hs@���@���@�j@�Q�@� �@�  @�  @�  @�  @���@��m@��w@���@�l�@�K�@�C�@�;d@�"�@��@��!@��@���@��@�7L@��9@��D@�Q�@�1'@� �@�  @�w@+@~�y@~ȴ@~��@~�+@~ff@~E�@~5?@~5?@}�T@}p�@}V@|�/@|�@|�@|j@{33@zJ@x�`@x1'@xbN@x��@x��@x�u@x��@x��@x�9@x�9@x�9@x�@xr�@x�@x��@xr�@x1'@w��@v�y@v��@vE�@u�-@u�-@u��@u�-@u�@up�@up�@up�@u`B@u?}@u/@t��@t��@t��@t��@t�/@t��@t�j@t�@t��@t�D@tZ@s�m@s��@s33@s@r��@q�^@q7L@q7L@pĜ@pbN@o|�@n��@n��@n��@n��@nff@l��@kdZ@j��@i�#@i��@h�9@hb@g�@e�@e�@ep�@eO�@d��@d��@d��@d�D@dj@dZ@d(�@d1@c��@c@b��@b�!@b�!@b�\@b�\@b=q@ahs@` �@]��@]�@\��@\��@\��@\��@\�@\�@\�@\�/@\�@[�F@[33@Z�!@Z-@Y�#@YG�@X��@Xr�@W�P@W;d@V�@V�R@V��@Vv�@V5?@U�T@U�h@Up�@UV@T��@UV@UV@UV@T�@T�/@T�j@TZ@Sƨ@S"�@R�H@R�!@R�\@R=q@Q��@P�`@P�9@P�9@P�9@P�@O|�@O\)@N�@M�h@M��@M@M��@M��@L�/@Lz�@L9X@L�@L9X@K��@KS�@J�H@Ix�@H�9@H�@Hr�@HQ�@H1'@H  @G��@F��@Fȴ@Fv�@FE�@FE�@FE�@F{@E�@D��@C�m@C33@B��@Bn�@Bn�@Bn�@B~�@B~�@B~�@B~�@B^5@A��@AX@A&�@@��@@Ĝ@@��@@�u@@�@@r�@@1'@?�@?��@?\)@?;d@>��@>�+@>V@>{@=�T@=��@=`B@=?}@<�@;�@;��@;��@;dZ@;C�@;33@;o@:�H@:��@:�!@:^5@:=q@:=q@:=q@:=q@:=q@:M�@:M�@:M�@:=q@:�@:J@9��@9��@9�@9��@9�@8�@8 �@8 �@8  @8  @7�@7�@7�@7�;@7�;@7��@7�w@7�@7�P@7l�@7\)@6��@5�@4��@4j@4Z@4(�@3�F@2�!@2^5@2M�@2-@1��@1G�@0�@0b@/�;@/�w@/��@/�P@/�P@/K�@/+@/+@/+@.��@.��@.��@.�+@.$�@-�@,�@,9X@+ƨ@+��@+t�@+t�@+S�@+33@+"�@+o@*��@*^5@)��@*J@*-@)�^@)��@)��@)��@)�7@)x�@)7L@)%@(��@(��@(�9@(1'@'K�@'�@&��@&ff@&$�@%p�@$�j@$��@$�D@$z�@$Z@#�F@#�@#dZ@#o@#o@#o@#@"�H@"��@"n�@"J@!��@!&�@ ��@ bN@   @��@�P@;d@��@��@?}@?}@/@�@V@��@�@�/@��@9X@ƨ@t�@"�@o@�H@��@��@n�@M�@=q@-@-@J@��@�#@��@��@��@1'@�@��@K�@+@��@�@�R@v�@E�@5?@$�@{@@�T@�@/@�@��@j@�m@�F@�@t�@t�@t�@dZ@S�@33@"�@o@o@o@�!@~�@�@�@�#@�^@��@&�@�u@b@  @�@  @�@��@��@�w@�@�@�w@�w@�w@�@�@�@��@�P@K�@�@�y@v�@$�@{@�@�h@`B@V@�/@��@j@�@�m@�F@dZ@S�@C�@S�@33@
�H@
��@
��@
��@
n�@
M�@
M�@
=q@
-@
�@
�@
J@
J@	�@	�#@	�#@	��@	��@	��@	��@	��@	��@	��@	�^@	��@	��@	x�@	7L@	�@	�@	�@��@�`@Ĝ@Ĝ@�9@�u@r�@bN@bN@b@|�@;d@�@
=@��@�y@�y@�y@�y@�@ȴ@�R@��@�+@E�@�@��@�-@�@p�@�@�@��@z�@j@j@j@Z@Z@I�@9X@I�@I�@I�@(�@��@��@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A˟�A˕�A�hsA�jA�hsA�ffA�dZA�bNA�bNA�bNA�^5A�bNA�^5A�ZA�ZA�ZA�ZA�ZA�VA�C�A��A��/AʶFAʑhA�`BA�1'A�hsA���A���A�bNA�VA��hA��-A�x�A��HA�A�A�33A�~�A�JA�bNA��FA��RA��RA��
A�x�A�t�A�  A��RA�l�A���A��\A��\A���A��A�t�A���A�%A���A���A�l�A��A�r�A��mA�x�A���A�bNA�33A��/A��7A�|�A�t�A�7LA��uA���A�A�ZA�1A��^A�{A���A�dZA�JA�z�A�^5A�%A�~�A�-A�A��mA���A�ffA���A���A�bA� �A�p�A�VA��A~��A}�7A{��A{Ayp�Aw��Au�As�Ar�Aq�;Ap��Ap�An�HAm�Al�!AlA�Ak;dAi��Ah��Ag�Ag�#AgƨAg�-Agx�Ae|�Ad�HAdn�Ad�Ac��Aa��A`�HA_A^~�A]��A]hsA[�;AY��AXZAV�+AS��AQ�FAP�AO�AO�ANbNAM��AM|�AM&�AL�jALffAL�AK�hAJ�AF�AE/ADA�AC33AA7LA@��A@ZA@=qA?�A?%A<ȴA;��A:�!A9�wA9�A8Q�A7�A6�yA5\)A4�9A4M�A3��A2�HA2A1t�A133A0�yA0~�A/�#A.n�A+��A*�!A*5?A)�7A)XA)�A(ȴA(-A'��A'XA&r�A$A"ĜA!�#A ��A��A�HA-A��A�A�9AM�A�A�A�A��A�A`BAS�A/A�9AS�A�A�AoA{A��AjAA�TAG�A%A�!A��A�+AVA(�A�A`BA
��A
I�AO�A��A�A�-A�yA�@�;d@�V@�b@�V@�`B@��@�V@�?}@���@�Z@�  @�v�@�@�K�@�9@���@�\@��#@��@�I�@�dZ@�J@�Z@��#@�1'@ߝ�@ޗ�@���@�1'@���@�E�@�p�@�Q�@���@�E�@Դ9@�l�@җ�@�ff@Ѻ^@�A�@��y@���@�I�@�b@���@�dZ@��@���@ư!@š�@�dZ@�V@�1@���@�I�@� �@�ƨ@�+@�-@���@���@�l�@�;d@�+@�o@�ȴ@���@�@���@���@�v�@�X@��`@��`@��`@��`@��@�Z@���@��@�x�@�&�@�%@��D@�b@�
=@�ff@�`B@�Ĝ@�A�@���@�;d@��!@�-@��R@���@�33@�@���@��`@��@���@��9@�b@���@�bN@�\)@���@���@�x�@�A�@��
@���@�\)@��@��@�X@��@��@�bN@��F@�;d@���@��!@�V@�$�@�7L@�9X@�"�@��R@�^5@�J@��#@�@��h@�p�@�O�@�7L@��@�%@���@��@���@��@�j@�I�@�9X@� �@�  @��@��;@���@��@�dZ@���@��H@��!@��\@�ff@�-@��@��T@���@�@���@�G�@���@�dZ@�33@��@�=q@��`@��`@���@�%@�%@��9@�|�@�\)@�;d@�"�@��y@�{@�hs@���@���@�j@�Q�@� �@�  @�  @�  @�  @���@��m@��w@���@�l�@�K�@�C�@�;d@�"�@��@��!@��@���@��@�7L@��9@��D@�Q�@�1'@� �@�  @�w@+@~�y@~ȴ@~��@~�+@~ff@~E�@~5?@~5?@}�T@}p�@}V@|�/@|�@|�@|j@{33@zJ@x�`@x1'@xbN@x��@x��@x�u@x��@x��@x�9@x�9@x�9@x�@xr�@x�@x��@xr�@x1'@w��@v�y@v��@vE�@u�-@u�-@u��@u�-@u�@up�@up�@up�@u`B@u?}@u/@t��@t��@t��@t��@t�/@t��@t�j@t�@t��@t�D@tZ@s�m@s��@s33@s@r��@q�^@q7L@q7L@pĜ@pbN@o|�@n��@n��@n��@n��@nff@l��@kdZ@j��@i�#@i��@h�9@hb@g�@e�@e�@ep�@eO�@d��@d��@d��@d�D@dj@dZ@d(�@d1@c��@c@b��@b�!@b�!@b�\@b�\@b=q@ahs@` �@]��@]�@\��@\��@\��@\��@\�@\�@\�@\�/@\�@[�F@[33@Z�!@Z-@Y�#@YG�@X��@Xr�@W�P@W;d@V�@V�R@V��@Vv�@V5?@U�T@U�h@Up�@UV@T��@UV@UV@UV@T�@T�/@T�j@TZ@Sƨ@S"�@R�H@R�!@R�\@R=q@Q��@P�`@P�9@P�9@P�9@P�@O|�@O\)@N�@M�h@M��@M@M��@M��@L�/@Lz�@L9X@L�@L9X@K��@KS�@J�H@Ix�@H�9@H�@Hr�@HQ�@H1'@H  @G��@F��@Fȴ@Fv�@FE�@FE�@FE�@F{@E�@D��@C�m@C33@B��@Bn�@Bn�@Bn�@B~�@B~�@B~�@B~�@B^5@A��@AX@A&�@@��@@Ĝ@@��@@�u@@�@@r�@@1'@?�@?��@?\)@?;d@>��@>�+@>V@>{@=�T@=��@=`B@=?}@<�@;�@;��@;��@;dZ@;C�@;33@;o@:�H@:��@:�!@:^5@:=q@:=q@:=q@:=q@:=q@:M�@:M�@:M�@:=q@:�@:J@9��@9��@9�@9��@9�@8�@8 �@8 �@8  @8  @7�@7�@7�@7�;@7�;@7��@7�w@7�@7�P@7l�@7\)@6��@5�@4��@4j@4Z@4(�@3�F@2�!@2^5@2M�@2-@1��@1G�@0�@0b@/�;@/�w@/��@/�P@/�P@/K�@/+@/+@/+@.��@.��@.��@.�+@.$�@-�@,�@,9X@+ƨ@+��@+t�@+t�@+S�@+33@+"�@+o@*��@*^5@)��@*J@*-@)�^@)��@)��@)��@)�7@)x�@)7L@)%@(��@(��@(�9@(1'@'K�@'�@&��@&ff@&$�@%p�@$�j@$��@$�D@$z�@$Z@#�F@#�@#dZ@#o@#o@#o@#@"�H@"��@"n�@"J@!��@!&�@ ��@ bN@   @��@�P@;d@��@��@?}@?}@/@�@V@��@�@�/@��@9X@ƨ@t�@"�@o@�H@��@��@n�@M�@=q@-@-@J@��@�#@��@��@��@1'@�@��@K�@+@��@�@�R@v�@E�@5?@$�@{@@�T@�@/@�@��@j@�m@�F@�@t�@t�@t�@dZ@S�@33@"�@o@o@o@�!@~�@�@�@�#@�^@��@&�@�u@b@  @�@  @�@��@��@�w@�@�@�w@�w@�w@�@�@�@��@�P@K�@�@�y@v�@$�@{@�@�h@`B@V@�/@��@j@�@�m@�F@dZ@S�@C�@S�@33@
�H@
��@
��@
��@
n�@
M�@
M�@
=q@
-@
�@
�@
J@
J@	�@	�#@	�#@	��@	��@	��@	��@	��@	��@	��@	�^@	��@	��@	x�@	7L@	�@	�@	�@��@�`@Ĝ@Ĝ@�9@�u@r�@bN@bN@b@|�@;d@�@
=@��@�y@�y@�y@�y@�@ȴ@�R@��@�+@E�@�@��@�-@�@p�@�@�@��@z�@j@j@j@Z@Z@I�@9X@I�@I�@I�@(�@��@��@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B��BB	7BDBJB	7B��B�B�B�B�B �B�BuB��BDB��B�B(�B�B�B�)B/B+B�B�B#�B�B�B&�B!�BuB%B+B+B��B��B��B��B�B�B��B�}B�B�FB�B�B�B��B��B��B��B�+Bm�BP�B:^B(�B�B�B�B{B	7B
�yB
�#B
��B
��B
��B
��B
��B
ȴB
��B
�3B
��B
��B
�JB
�7B
�\B
�7B
y�B
p�B
bNB
_;B
VB
@�B
0!B
&�B
%�B
�B
�B
VB
%B
  B	��B	��B	�B	�BB	�TB	�NB	�yB	�sB	�`B	�5B	��B	��B	��B	��B	ĜB	�3B	�B	��B	��B	��B	��B	�DB	x�B	t�B	iyB	VB	O�B	T�B	P�B	J�B	J�B	F�B	E�B	C�B	@�B	=qB	8RB	.B	�B��B	+B	%B	B��B��B	  B��B��B�yB�B�B�B��B�
B��B��BɺB��BɺB��BƨB��B�}B��BÖB��B�dB�9B��B��B��B��B��B��B��B��B��B��B��B�PBw�B~�B~�B{�Bu�Bx�Bw�Bx�Bw�Bw�Bu�Bu�Bo�Be`Bl�Bm�Bo�Bn�BhsB]/BN�BA�BJ�BQ�BM�BM�BD�BA�BC�BL�BQ�BP�BR�BR�BO�BL�BG�BC�B>wB/B�B%�B/B$�B-B �B#�B33B-B,B5?B1'B/B5?B:^B:^B9XB33B,B33B(�B/B:^B8RB6FB5?B1'B-B%�B%�B'�B33B/B/B(�B.B1'B/B-B+B.B)�B+B0!B49B0!B)�B,B'�B7LB:^B8RB5?B2-B'�B�B)�B$�B"�B1'B&�B9XB>wB<jB:^B9XB9XB@�BD�BI�BI�BI�BG�BF�BB�B?}BB�BD�BJ�BP�BW
BVBT�BS�BN�BL�BK�BT�BZB[#BYBYBXB\)B]/BdZBhsBhsBk�BjBn�B{�B�B�B{�B~�B�B�+B�%B�B|�Bk�Bx�B|�B�B�1B�%B�B�PB�bB�VB�PB�JB�uB��B��B��B��B��B��B��B��B��B��B��B�'B�dB�}BBƨBǮBȴB��B��B��B��B��B��B��B��B��B��B��B�B�B�
B�B�B�B�B�#B�B�;B�;B�HB�NB�TB�`B�mB�sB�mB�fB�ZB�HB�B��B��B�B��B	%B	
=B	
=B	
=B		7B	%B	bB	hB	bB	VB	PB	�B	�B	#�B	(�B	,B	-B	/B	2-B	33B	33B	33B	33B	33B	49B	5?B	7LB	9XB	:^B	:^B	;dB	<jB	@�B	K�B	Q�B	T�B	ZB	^5B	_;B	aHB	dZB	dZB	ffB	gmB	o�B	u�B	w�B	z�B	}�B	� B	�B	�B	�B	�B	�B	�B	�%B	�%B	�B	�B	�1B	�VB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	��B	��B	��B	��B	�'B	�'B	�-B	�?B	�?B	�?B	�?B	�FB	�FB	�FB	�FB	�FB	�LB	�LB	�RB	�RB	�RB	�RB	�XB	�XB	�^B	�^B	�^B	�^B	�^B	�jB	�qB	�wB	�}B	�qB	��B	ĜB	B	ÖB	B	ƨB	ȴB	��B	ɺB	ƨB	B	ȴB	��B	��B	��B	��B	��B	��B	��B	�B	�)B	�#B	�#B	�#B	�)B	�)B	�)B	�)B	�)B	�)B	�#B	�#B	�5B	�;B	�;B	�5B	�5B	�)B	�B	�B	�B	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
%B
%B
%B
%B
B
B
B
%B
+B
	7B
PB
PB
VB
PB
JB
VB
hB
oB
oB
bB
PB
\B
\B
PB
uB
�B
�B
�B
uB
{B
{B
�B
�B
{B
{B
oB
\B
bB
{B
uB
uB
uB
oB
oB
bB
uB
uB
{B
�B
{B
uB
oB
oB
oB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
 �B
 �B
 �B
 �B
�B
�B
%�B
'�B
'�B
)�B
+B
+B
+B
+B
+B
,B
.B
/B
/B
/B
/B
/B
/B
/B
/B
/B
0!B
0!B
0!B
0!B
/B
.B
/B
2-B
49B
49B
49B
49B
49B
49B
49B
49B
49B
49B
49B
49B
49B
33B
2-B
0!B
33B
5?B
8RB
7LB
7LB
6FB
9XB
;dB
:^B
:^B
9XB
:^B
<jB
=qB
>wB
>wB
?}B
?}B
>wB
?}B
?}B
?}B
?}B
>wB
?}B
?}B
>wB
=qB
>wB
@�B
B�B
C�B
D�B
D�B
D�B
D�B
E�B
E�B
D�B
D�B
E�B
G�B
G�B
E�B
G�B
G�B
G�B
G�B
G�B
G�B
F�B
G�B
H�B
G�B
F�B
E�B
H�B
I�B
G�B
H�B
F�B
H�B
K�B
K�B
K�B
K�B
J�B
L�B
M�B
M�B
N�B
N�B
N�B
M�B
M�B
M�B
M�B
M�B
M�B
N�B
O�B
P�B
P�B
P�B
P�B
O�B
P�B
S�B
VB
W
B
W
B
W
B
W
B
W
B
W
B
VB
VB
VB
W
B
W
B
YB
YB
YB
YB
YB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
YB
XB
XB
ZB
[#B
[#B
\)B
\)B
\)B
]/B
\)B
]/B
^5B
^5B
^5B
^5B
]/B
\)B
]/B
^5B
^5B
]/B
^5B
`BB
`BB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
bNB
bNB
aHB
`BB
aHB
aHB
bNB
cTB
bNB
bNB
aHB
bNB
cTB
e`B
ffB
ffB
ffB
e`B
ffB
ffB
ffB
ffB
ffB
ffB
ffB
ffB
ffB
ffB
ffB
e`B
e`B
e`B
ffB
e`B
ffB
gmB
gmB
ffB
gmB
gmB
hsB
hsB
hsB
hsB
iyB
iyB
iyB
k�B
k�B
k�B
jB
jB
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
l�B
l�B
l�B
l�B
m�B
n�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
m�B
l�B
n�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
r�B
q�B
r�B
r�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
s�B
s�B
r�B
u�B
u�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�}B��B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B��B�8B�B	�B�B�B
#B;B�?B�B�B�B"�B �BB��BhB�B�B,WB!�B�B��B0�B-CB"�B�B%`ByB1B'�B#:B�BfB�BfB��B�$B��B�	B��B��B��B��B��B�fB�WB��B��B��B�$B�&B��B�BpoBUB=�B,B \B 'B�BgB
�B
�B
��B
͟B
�B
ңB
�TB
�HB
�lB
��B
�%B
�B
�)B
�pB
��B
��B
�	B
{�B
r|B
d@B
`�B
W�B
B�B
2�B
(sB
'B
B
�B
�B
�B
;B	�8B	��B	�B	�4B	�tB	�B	�B	�B	�B	��B	��B	͟B	�hB	�dB	ŢB	�ZB	�}B	��B	��B	��B	��B	�jB	{�B	v�B	l"B	YeB	R B	VB	R B	K�B	K�B	G_B	F%B	DB	A B	=�B	8�B	/OB		B	B	�B	zB	�B��B��B	 iB�]B��B�B��B�qBچB�MB��B��B��B�DB�{BʌB�xBǮB��B��B�AB�B�B�6B�tB�$B�eB�-B��B��B�XB�RB�nB�pB�WB�B��Bz�B��B�OB}qBwLBy�Bx�By�Bx�BxlBv`BvFBpoBf�BmCBn/Bo�Bn�Bh�B^OBQBD�BLJBR�BO(BN�BFYBCaBEBM�BRTBQhBS&BS@BPHBM6BHfBD3B?}B0�BKB'�B0�B&�B.cB#:B%�B4B.�B-CB5�B2-B0;B5�B:�B:�B9�B4nB-wB49B*�B0;B:�B8�B6�B5�B1�B./B'RB'�B)B3�B/�B/�B)�B.�B1�B/�B-�B,B.�B+B+�B0�B4�B0�B+6B-CB)DB7�B:�B8�B5�B2�B)B�B+B&�B$�B1�B(�B9�B>�B<�B:�B:*B:DBABEBI�BI�BI�BG�BF�BC-B@iBC{BE�BK�BQ4BW
BVBUBT,BOvBM�BL�BUgBZkB[WBY�BY�BX�B\�B^Bd�Bh�Bh�Bk�BkBn�B{�B��B�;B|�B}B�B�EB�YB�gB}�Bm�ByXB}�B��B��B�B�B��B��B��B��B�B��B��B��B�B�B�'B�&B�B�FB�2B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B��B�B�B�B�B�B�$B�+B�+B�EB�7B�WB�kB�;B�pB�bB�B�nB�B�B�B�B�B��B�4B��B��B�B�TB�tB	%B	
=B	
XB	
rB		�B	�B	}B	�B	�B	�B	B	
B	 B	$&B	)*B	,"B	-CB	/B	2-B	3B	3MB	3MB	33B	3MB	4TB	5ZB	7fB	9XB	:xB	:xB	;�B	<�B	@�B	LB	R B	UMB	ZkB	^OB	_pB	abB	dtB	d�B	f�B	g�B	o�B	u�B	w�B	z�B	~B	�B	�'B	�-B	�AB	�GB	�MB	�9B	�?B	�%B	�SB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	��B	�B	�0B	�0B	�KB	�AB	�[B	�aB	�%B	�?B	�?B	�ZB	�FB	�+B	�FB	�`B	�`B	�fB	�LB	�RB	�8B	�RB	�lB	�rB	�rB	�^B	�xB	�xB	�xB	��B	��B	��B	��B	��B	��B	��B	ĶB	��B	��B	��B	��B	��B	ʦB	��B	��B	�-B	�7B	�(B	�4B	�B	�NB	�,B	�aB	�gB	�7B	�)B	�=B	�=B	�=B	�CB	�CB	�)B	�CB	�CB	�CB	�WB	�qB	�OB	�;B	�!B	�5B	�5B	�]B	�B	ٚB	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B
 B
B
-B
B
%B
%B
%B
9B
9B
B
YB
_B
	lB
jB
jB
pB
�B
�B
pB
�B
oB
oB
}B
�B
vB
vB
�B
uB
�B
�B
�B
�B
�B
�B
mB
�B
�B
�B
�B
�B
�B
{B
�B
�B
�B
�B
�B
�B
uB
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
 �B
 �B
 �B
 �B
B
B
%�B
'�B
'�B
*B
+B
+B
+B
+B
+B
,"B
./B
/B
/B
/5B
/B
/B
/ B
/B
/B
/5B
0!B
0B
0!B
0;B
/5B
.IB
/5B
2GB
49B
49B
49B
49B
49B
49B
49B
4B
49B
4B
4TB
4nB
4TB
3MB
2aB
0oB
3hB
5tB
8lB
7fB
7�B
6�B
9XB
;dB
:xB
:�B
9�B
:�B
<�B
=�B
>�B
>�B
?}B
?}B
>�B
?}B
?}B
?cB
?�B
>�B
?�B
?�B
>�B
=�B
>�B
@�B
B�B
C�B
D�B
D�B
D�B
D�B
E�B
E�B
D�B
D�B
E�B
G�B
G�B
E�B
G�B
G�B
G�B
G�B
G�B
G�B
F�B
G�B
H�B
G�B
F�B
E�B
H�B
I�B
G�B
H�B
F�B
H�B
K�B
K�B
K�B
K�B
J�B
L�B
M�B
M�B
N�B
N�B
N�B
M�B
M�B
M�B
NB
NB
NB
N�B
O�B
Q B
P�B
QB
QB
P.B
Q4B
TB
VB
W
B
V�B
W
B
V�B
W
B
W
B
VB
V9B
V9B
W$B
W$B
YB
Y1B
YB
Y1B
Y1B
Z7B
ZB
ZB
ZB
Z7B
ZB
Z7B
Z7B
YB
X_B
XEB
Z7B
[=B
[=B
\CB
\CB
\CB
]IB
\CB
]IB
^5B
^OB
^5B
^5B
]IB
\]B
]/B
^OB
^5B
]dB
^jB
`\B
`\B
aHB
aHB
aHB
aHB
a-B
abB
aHB
bNB
bNB
abB
`vB
abB
abB
bhB
cTB
bhB
bhB
a|B
b�B
c�B
e`B
ffB
ffB
f�B
ezB
ffB
ffB
ffB
ffB
ffB
ffB
ffB
ffB
fLB
ffB
ffB
ezB
e`B
ezB
f�B
e�B
f�B
gmB
g�B
f�B
g�B
g�B
h�B
h�B
h�B
h�B
i�B
i�B
i�B
k�B
k�B
k�B
j�B
j�B
kkB
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
lqB
l�B
m�B
mwB
m�B
m�B
m�B
m�B
m�B
m�B
l�B
l�B
l�B
l�B
m�B
n�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
m�B
l�B
n�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
r�B
q�B
r�B
r�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
s�B
s�B
r�B
u�B
u�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.01(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201811050039412018110500394120181105003941201811050200172018110502001720181105020017201811060022122018110600221220181106002212  JA  ARFMdecpA19c                                                                20181101093530  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20181101003534  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20181101003537  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20181101003537  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20181101003538  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20181101003538  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20181101003538  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20181101003538  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20181101003539  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20181101003539                      G�O�G�O�G�O�                JA  ARUP                                                                        20181101005743                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20181101153251  CV  JULD            G�O�G�O�F�i�                JM  ARCAJMQC2.0                                                                 20181104153941  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20181104153941  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20181104170017  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20181105152212  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115131518                      G�O�G�O�G�O�                