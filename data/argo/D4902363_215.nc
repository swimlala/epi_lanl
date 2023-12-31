CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-03-03T00:35:13Z creation;2018-03-03T00:35:17Z conversion to V3.1;2019-12-19T07:48:06Z update;     
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
resolution        =���   axis      Z        \  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     \  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \,   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     \  `   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o`   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     \  s8   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     \  �l   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     \  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     \  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     \  �0   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     \  ˌ   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �x   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �x   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �x   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �x   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �    HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �H   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �X   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �\   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �l   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �p   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �t   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �xArgo profile    3.1 1.2 19500101000000  20180303003513  20200115121518  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0576_215                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @�Pu�<��1   @�Pvfff�@:He��O�dg���1   GPS     A   B   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De�fDf  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D��3D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�<�D�|�D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�C3D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D��3D��f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�Q�@�Q�A (�A (�A@(�A`(�A�{A�{A�{A�{A�{A�{A�{A�{B 
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
=Bp
=Bx
=B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�C �C�C�C�C�C
�C�C�C�C�C�C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HD  �D ��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D	 �D	��D
 �D
��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D�
D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D  �D ��D! �D!��D" �D"��D# �D#��D$ �D$��D% �D%��D& �D&��D' �D'��D( �D(��D) �D)��D* �D*��D+ �D+��D, �D,��D- �D-��D. �D.��D/ �D/��D0 �D0��D1 �D1��D2 �D2��D3 �D3��D4 �D4��D5 �D5��D6 �D6��D7 �D7��D8 �D8��D9 �D9��D: �D:��D; �D;��D< �D<��D= �D=��D> �D>��D? �D?��D@ �D@��DA �DA��DB �DB��DC �DC��DD �DD��DE �DE��DF �DF��DG �DG��DH �DH��DI �DI��DJ �DJ��DK �DK��DL �DL��DM �DM��DN �DN��DO �DO��DP �DP��DQ �DQ��DR �DR��DS �DS��DT �DT��DU �DU��DV �DV��DW �DW��DX �DX��DY �DY��DZ �DZ��D[ �D[��D\ �D\��D] �D]��D^ �D^��D_ �D_��D` �D`��Da �Da��Db �Db��Dc �Dc��Dd �Dd��De �De�
Df �Df��Dg �Dg��Dh �Dh��Di �Di��Dj �Dj��Dk �Dk��Dl �Dl��Dm �Dm��Dn �Dn��Do �Do��Dp �Dp��Dq �Dq��Dr �Dr��Ds �Ds��Dt �Dt��Du �Du��Dv �Dv��Dw �Dw��Dx �Dx��Dy �Dy��Dz �Dz��D{ �D{��D| �D|��D} �D}��D~ �D~��D �D��D� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD��D�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�C�D���D�ÅD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD���D�ÅD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD�ÅD� RD�=D��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RDRD��RD� RD�@RDÀRD��RD� RD�@RDĀRD��RD� RD�@RDŀRD��RD� RD�@RDƀRD��RD� RD�@RDǀRD��RD� RD�@RDȀRD��RD� RD�@RDɀRD��RD� RD�@RDʀRD��RD� RD�@RDˀRD��RD� RD�@RD̀RD��RD� RD�@RD̀RD��RD� RD�@RD΀RD��RD� RD�@RDπRD��RD� RD�@RDЀRD��RD� RD�@RDрRD��RD� RD�@RDҀRD��RD� RD�@RDӀRD��RD� RD�@RDԀRD��RD� RD�=D�}D��RD� RD�@RDրRD��RD� RD�@RD׀RD��RD� RD�C�D؀RD��RD� RD�@RDـRD��RD� RD�@RDڀRD��RD� RD�@RDۀRD��RD� RD�@RD܀RD��RD� RD�@RD݀RD��RD� RD�@RDހRD��RD� RD�@RD߀RD��RD� RD�@RD��RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD��RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD��RD�ÅD���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�G�A�C�A�I�A�/A�"�A��A��A�VA�A�A���A��A��A��A��A��TA���A���A���A�ƨA�ƨA���A��^A��A���A�~�A�ZA�$�A�
=A��
A���A���A�C�A�ȴA��A���A�{A��+A�K�A�VA���A�\)A�bA��yA��-A�n�A��A��TA��+A��A�jA�ZA��A�A�z�A�`BA��uA�x�A���A�Q�A�Q�A��A�JA���A��TA���A�|�A��A�5?A�z�A��A�+A��\A��uA�A�n�A���A�dZA�
=A�~�A� �A�9XA��A~E�Ay�hAw��Avz�Au�AtM�AshsArA�AqXAp�/Ao��AnQ�AmC�Ak;dAg�Ae�-AcK�Ab�`Ab^5Ab �Aa�TAa�wAa��Aa/A`z�A^E�A\�\A[+AZ��AZ��AZjAZ�AY�7AXE�AU�-AR�9AR�AQ`BAPz�APAPAO��AO�AO�FAO�hAOoAMoAL�+ALA�AK�AKt�AJ�`AJ  AI�AIoAH��AHVAG�AF�DAEC�ABZAAVA@jA>1A=dZA<ĜA<-A;��A:v�A9�-A933A8�jA8r�A89XA7�;A7`BA6�A4ȴA41A3�A3�;A3?}A2�!A1�mA1��A0~�A//A-�A+�;A+A*�9A*bNA)�wA(�\A&�/A&ZA&=qA%�TA%�hA%`BA$v�A#G�A"��A"v�A"A�A!��A!`BA �RAffA33A��A~�A�TA�Ax�A�!AffA{A��A�A�A�9AI�A�AC�A�uAbA/A5?AJA�mA7LA��A
=A�uAbA�A\)AC�A�A��A
r�A	\)A�/A�/A�`A�A��AdZA+AVA`BA��AJA��AK�A ~�A $�A (�@�ƨ@���@�hs@�bN@��@�hs@�Z@�+@�G�@�-@�z�@�w@��@��^@�@���@�+@��@���@��@ݙ�@ܣ�@�A�@�o@�hs@���@��@ӕ�@�X@�bN@�t�@�v�@�J@�Ĝ@�t�@�C�@��@�ff@���@��@�&�@�9X@ǥ�@�K�@ƸR@�v�@���@�J@��#@���@��@��@��`@�;d@�V@�{@�`B@�I�@��!@���@�j@�33@��@���@���@��@��D@�(�@��@�ƨ@�
=@��T@�X@���@���@��\@���@��@��;@�o@�=q@��-@��@�bN@�Q�@��@��H@�v�@�=q@���@���@���@��@�x�@�`B@��@���@�Z@�  @�K�@���@�V@��#@�O�@��`@�A�@��@���@���@��@�@��@��#@��^@�/@�  @��P@�o@���@��+@��^@��-@�X@��@��@��@�bN@�1'@���@��@�~�@��@�O�@�O�@�/@�;d@�-@���@���@�O�@���@��u@�z�@�A�@��@��@�33@��R@���@��@��@�Q�@�  @��;@��w@�|�@�;d@�o@�ȴ@��+@�@���@��h@���@�x�@�X@�V@��@�Q�@�b@��;@���@��F@��P@�
=@�~�@��@��^@���@��7@��h@��-@���@�$�@�{@��@��@���@�V@�I�@��;@��w@�t�@�K�@��@�@��@�o@�"�@�M�@��@���@���@���@�Ĝ@� �@�;@�w@�P@;d@~��@~E�@~5?@}�-@|�@{�m@{�
@{�
@{�
@{t�@y��@yX@x�`@xbN@w��@v��@v��@v5?@u��@t�@sƨ@sdZ@sC�@r�@r�H@s33@s"�@r�@r�@r�@r�@r�@r�@r��@r-@q��@q�7@p��@p�`@pĜ@p�9@p��@pĜ@p��@pbN@p�@pbN@pr�@p�u@p�u@pQ�@o
=@n5?@m@m�@mO�@m?}@m?}@l�@l�D@l9X@l1@l(�@lZ@l1@k��@j��@i��@iX@iG�@i&�@h��@hbN@g�P@gl�@g��@f�y@f�+@fE�@f@e��@e/@d��@dz�@d1@b~�@b-@a�@a��@a��@a�^@a��@a��@a&�@`�u@_�w@_+@^�@^��@^$�@^{@^@]�T@]�-@]`B@\�/@\j@\9X@[ƨ@[C�@["�@[@Z�H@Z�!@Zn�@Y�@Y��@Yx�@Y&�@X�9@XbN@XA�@W�@W�;@W��@Wl�@V��@VV@V5?@U@U�@T�@T�/@T�/@T�/@T�j@T�D@TZ@T(�@S��@Sƨ@S��@St�@SdZ@S33@RM�@Q�^@Qx�@Qhs@QX@QG�@Q7L@Q7L@Q&�@P��@P�`@P��@PĜ@PbN@P �@Pb@P  @O��@O�@O�@N�@Nȴ@Nv�@M�@M@M�@L�D@Lj@LZ@L(�@L(�@L�@K�m@K�@Ko@J��@J��@J�\@J~�@J~�@Jn�@Jn�@J^5@J-@I�@I��@I��@IX@IX@IG�@HĜ@HbN@G�;@G�@G��@G�P@G|�@G;d@F��@Fv�@FE�@F5?@F$�@E��@E�h@E�@EO�@E�@D�D@Dj@DZ@D(�@D�@D1@C�F@C��@CS�@B��@B^5@A��@Ahs@A7L@A%@@��@@Q�@?�;@?��@?K�@>ȴ@>ff@>$�@=�T@=?}@<j@;��@;ƨ@;dZ@:�@:^5@:�@9��@9��@9%@8r�@7�;@7��@7\)@7�@6��@6�R@6�+@65?@5�T@5��@5�@5p�@5`B@5O�@5V@4�j@4j@4j@4I�@4�@3�
@3t�@333@333@333@333@3"�@2�H@2�!@2~�@2-@1�#@1��@1�7@1�@0Ĝ@/�@/�@/��@/|�@/;d@/
=@.�y@.�R@.��@.5?@-�@-��@-p�@-`B@-?}@,��@,��@,Z@,(�@+��@+�m@+ƨ@+dZ@*�@*�\@*n�@*=q@)�#@)�7@)7L@)%@(�9@'�@'�w@'�P@'\)@&��@&ȴ@&��@&v�@&ff@&$�@%��@%��@%p�@%�@$��@$z�@$j@$9X@$1@#ƨ@#C�@#"�@#o@#o@"�@"�H@"^5@"�@!�@!�@!�@!�#@!�7@!7L@!%@ �9@ �u@ bN@�;@�w@�@��@l�@��@��@E�@�@��@p�@/@�@��@�@j@Z@I�@(�@��@�F@�@dZ@S�@S�@o@�@��@�!@�!@^5@��@�#@��@��@hs@G�@&�@��@��@Ĝ@Ĝ@�9@�@r�@Q�@Q�@A�@b@  @�;@�w@�@�P@|�@l�@\)@K�@�@�y@ȴ@��@v�@V@E�@5?@{@�@@�-@p�@V@�@�D@�@�
@�F@��@�@t�@dZ@t�@t�@dZ@S�@o@�!@~�@=q@�@��@�@��@��@�9@��@r�@1'@  @��@�P@\)@�@��@ff@{@�-@�h@?}@�@�D@(�@��@�m@�m@t�@@
��@
n�@
M�@
=q@
-@	�@	��@	��@	��@	�7@	X@	%@�9@��@�u@�@�@�@r�@Q�@b@�w@�@��@l�@�@ȴ@�R@�R@��@�+@E�@{@�@@�-@�@?}@��@�j@Z@��@��@ƨ@t�@dZ@S�@C�@33@o@�@�!@�!@��@M�@=q@�@��@��11111111111111111111111111111111111111111111111111144111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�G�A�C�A�I�A�/A�"�A��A��A�VA�A�A���A��A��A��A��A��TA���A���A���A�ƨA�ƨA���A��^A��A���A�~�A�ZA�$�A�
=A��
A���A���A�C�A�ȴA��A���A�{A��+A�K�A�VA���A�\)A�bA��yA��-A�n�A��A��TA��+A��A�jG�O�G�O�A�A�z�A�`BA��uA�x�A���A�Q�A�Q�A��G�O�G�O�A��TA���A�|�A��A�5?A�z�A��A�+A��\A��uA�A�n�A���A�dZA�
=A�~�A� �A�9XA��A~E�Ay�hAw��Avz�Au�AtM�AshsArA�AqXAp�/Ao��AnQ�AmC�Ak;dAg�Ae�-AcK�Ab�`Ab^5Ab �Aa�TAa�wAa��Aa/A`z�A^E�A\�\A[+AZ��AZ��AZjAZ�AY�7AXE�AU�-AR�9AR�AQ`BAPz�APAPAO��AO�AO�FAO�hAOoAMoAL�+ALA�AK�AKt�AJ�`AJ  AI�AIoAH��AHVAG�AF�DAEC�ABZAAVA@jA>1A=dZA<ĜA<-A;��A:v�A9�-A933A8�jA8r�A89XA7�;A7`BA6�A4ȴA41A3�A3�;A3?}A2�!A1�mA1��A0~�A//A-�A+�;A+A*�9A*bNA)�wA(�\A&�/A&ZA&=qA%�TA%�hA%`BA$v�A#G�A"��A"v�A"A�A!��A!`BA �RAffA33A��A~�A�TA�Ax�A�!AffA{A��A�A�A�9AI�A�AC�A�uAbA/A5?AJA�mA7LA��A
=A�uAbA�A\)AC�A�A��A
r�A	\)A�/A�/A�`A�A��AdZA+AVA`BA��AJA��AK�A ~�A $�A (�@�ƨ@���@�hs@�bN@��@�hs@�Z@�+@�G�@�-@�z�@�w@��@��^@�@���@�+@��@���@��@ݙ�@ܣ�@�A�@�o@�hs@���@��@ӕ�@�X@�bN@�t�@�v�@�J@�Ĝ@�t�@�C�@��@�ff@���@��@�&�@�9X@ǥ�@�K�@ƸR@�v�@���@�J@��#@���@��@��@��`@�;d@�V@�{@�`B@�I�@��!@���@�j@�33@��@���@���@��@��D@�(�@��@�ƨ@�
=@��T@�X@���@���@��\@���@��@��;@�o@�=q@��-@��@�bN@�Q�@��@��H@�v�@�=q@���@���@���@��@�x�@�`B@��@���@�Z@�  @�K�@���@�V@��#@�O�@��`@�A�@��@���@���@��@�@��@��#@��^@�/@�  @��P@�o@���@��+@��^@��-@�X@��@��@��@�bN@�1'@���@��@�~�@��@�O�@�O�@�/@�;d@�-@���@���@�O�@���@��u@�z�@�A�@��@��@�33@��R@���@��@��@�Q�@�  @��;@��w@�|�@�;d@�o@�ȴ@��+@�@���@��h@���@�x�@�X@�V@��@�Q�@�b@��;@���@��F@��P@�
=@�~�@��@��^@���@��7@��h@��-@���@�$�@�{@��@��@���@�V@�I�@��;@��w@�t�@�K�@��@�@��@�o@�"�@�M�@��@���@���@���@�Ĝ@� �@�;@�w@�P@;d@~��@~E�@~5?@}�-@|�@{�m@{�
@{�
@{�
@{t�@y��@yX@x�`@xbN@w��@v��@v��@v5?@u��@t�@sƨ@sdZ@sC�@r�@r�H@s33@s"�@r�@r�@r�@r�@r�@r�@r��@r-@q��@q�7@p��@p�`@pĜ@p�9@p��@pĜ@p��@pbN@p�@pbN@pr�@p�u@p�u@pQ�@o
=@n5?@m@m�@mO�@m?}@m?}@l�@l�D@l9X@l1@l(�@lZ@l1@k��@j��@i��@iX@iG�@i&�@h��@hbN@g�P@gl�@g��@f�y@f�+@fE�@f@e��@e/@d��@dz�@d1@b~�@b-@a�@a��@a��@a�^@a��@a��@a&�@`�u@_�w@_+@^�@^��@^$�@^{@^@]�T@]�-@]`B@\�/@\j@\9X@[ƨ@[C�@["�@[@Z�H@Z�!@Zn�@Y�@Y��@Yx�@Y&�@X�9@XbN@XA�@W�@W�;@W��@Wl�@V��@VV@V5?@U@U�@T�@T�/@T�/@T�/@T�j@T�D@TZ@T(�@S��@Sƨ@S��@St�@SdZ@S33@RM�@Q�^@Qx�@Qhs@QX@QG�@Q7L@Q7L@Q&�@P��@P�`@P��@PĜ@PbN@P �@Pb@P  @O��@O�@O�@N�@Nȴ@Nv�@M�@M@M�@L�D@Lj@LZ@L(�@L(�@L�@K�m@K�@Ko@J��@J��@J�\@J~�@J~�@Jn�@Jn�@J^5@J-@I�@I��@I��@IX@IX@IG�@HĜ@HbN@G�;@G�@G��@G�P@G|�@G;d@F��@Fv�@FE�@F5?@F$�@E��@E�h@E�@EO�@E�@D�D@Dj@DZ@D(�@D�@D1@C�F@C��@CS�@B��@B^5@A��@Ahs@A7L@A%@@��@@Q�@?�;@?��@?K�@>ȴ@>ff@>$�@=�T@=?}@<j@;��@;ƨ@;dZ@:�@:^5@:�@9��@9��@9%@8r�@7�;@7��@7\)@7�@6��@6�R@6�+@65?@5�T@5��@5�@5p�@5`B@5O�@5V@4�j@4j@4j@4I�@4�@3�
@3t�@333@333@333@333@3"�@2�H@2�!@2~�@2-@1�#@1��@1�7@1�@0Ĝ@/�@/�@/��@/|�@/;d@/
=@.�y@.�R@.��@.5?@-�@-��@-p�@-`B@-?}@,��@,��@,Z@,(�@+��@+�m@+ƨ@+dZ@*�@*�\@*n�@*=q@)�#@)�7@)7L@)%@(�9@'�@'�w@'�P@'\)@&��@&ȴ@&��@&v�@&ff@&$�@%��@%��@%p�@%�@$��@$z�@$j@$9X@$1@#ƨ@#C�@#"�@#o@#o@"�@"�H@"^5@"�@!�@!�@!�@!�#@!�7@!7L@!%@ �9@ �u@ bN@�;@�w@�@��@l�@��@��@E�@�@��@p�@/@�@��@�@j@Z@I�@(�@��@�F@�@dZ@S�@S�@o@�@��@�!@�!@^5@��@�#@��@��@hs@G�@&�@��@��@Ĝ@Ĝ@�9@�@r�@Q�@Q�@A�@b@  @�;@�w@�@�P@|�@l�@\)@K�@�@�y@ȴ@��@v�@V@E�@5?@{@�@@�-@p�@V@�@�D@�@�
@�F@��@�@t�@dZ@t�@t�@dZ@S�@o@�!@~�@=q@�@��@�@��@��@�9@��@r�@1'@  @��@�P@\)@�@��@ff@{@�-@�h@?}@�@�D@(�@��@�m@�m@t�@@
��@
n�@
M�@
=q@
-@	�@	��@	��@	��@	�7@	X@	%@�9@��@�u@�@�@�@r�@Q�@b@�w@�@��@l�@�@ȴ@�R@�R@��@�+@E�@{@�@@�-@�@?}@��@�j@Z@��@��@ƨ@t�@dZ@S�@C�@33@o@�@�!@�!@��@M�@=q@�@��@��11111111111111111111111111111111111111111111111111144111111111441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�!B�!B�B�'B�3B�?B�9B�FB�RB�XB�^B�dB�qB�qB�dB�dB��B�}B��B��BB��BBB��B��BBƨBŢB��BƨBĜBĜB��B��BƨB�qBÖB��B�XB�?B�-B�-B�B��B��B��B��B��B�7B�{B~�B<jB	7B�BB��B�+B��B�BffBN�BI�B%BBB
��BB
�B
ĜB
��B
ȴB
�LB
�?B
�B
��B
��B
�uB
ffB
H�B
Q�B
W
B
7LB
B

=B
�B
�B
VB
B
B	��B	��B	�mB	��B	��B	�!B	�%B	�uB	�uB	�RB	�9B	�FB	�?B	�'B	�B	��B	�\B	n�B	r�B	r�B	�1B	�%B	|�B	u�B	hsB	P�B	2-B	�B	J�B	H�B	E�B	H�B	R�B	Q�B	N�B	H�B	B�B	8RB	�B	33B	9XB	6FB	/B	/B	#�B	&�B	$�B	 �B	�B��B��B�`B�}B�B�5B��B�ZB�TB�;B�/B��B��B�)B�/B�#B�)B��BɺBB��B�wB��B��B�dB�XB�B�B��B�JB�hB|�B��B��B��B�uB�=B�B�uB��B��B��B��B�1B|�B�%B�\B�DB� B�Br�BS�B`BBt�Bo�BaHBZBL�B^5BhsBhsBffBe`B`BB^5B[#BW
BO�BL�BJ�BE�BA�BQ�BL�B=qB-B:^BB�BB�BB�BB�BB�B6FB"�B�B+B5?BC�B?}B6FB(�B6FB1'B �B�B�B{BbB%B�B+B2-B+B�B$�B�B�B�B�BuBPB��BPB�BuBVBB	7B�B�B��BBJBbBuBJBB+BBBDB�B�B�B �B�B�B%�B%�B �B�B �B&�B�B�B"�B�B!�B�B#�B�BuBDB�B�B�B$�B-B'�B!�B!�B$�B'�B+B-B9XB<jB9XB7LB;dB<jB;dB5?B0!B7LB8RB2-B/B:^B:^B7LB?}BB�BG�BE�BS�BS�BO�BJ�BW
B[#B\)B]/B^5B_;B`BB_;B\)B[#B]/B]/BYB_;B`BBbNBdZBffBhsBm�Bm�Bl�Bo�By�By�Bw�Bu�Bp�Bm�B{�B~�B�B�B� B�PB�DB�VB�bB�bB�bB�oB�bB�PB�7B�bB��B��B��B�JB��B�B�3B�-B�-B�LB�XB�RB�LB�^B�XB�dB��B�qB�jBĜB��B��B��B��B�B�B�
B�
B��B�BB�TB�ZB�TB�ZB�ZB�ZB�B�B�B�B�B�B�B�B��B	  B	B	1B	
=B	\B	\B	�B	�B	�B	�B	�B	�B	�B	�B	#�B	"�B	%�B	'�B	+B	-B	2-B	6FB	33B	;dB	@�B	@�B	H�B	J�B	G�B	R�B	S�B	S�B	R�B	T�B	VB	^5B	^5B	_;B	cTB	hsB	iyB	hsB	ffB	cTB	k�B	n�B	n�B	m�B	o�B	p�B	p�B	p�B	r�B	s�B	}�B	�B	�B	�%B	�=B	�=B	�=B	�DB	�JB	�JB	�JB	�JB	�=B	�7B	�DB	�JB	�\B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�-B	�'B	�'B	�B	�B	�?B	�XB	�RB	�LB	�^B	�^B	�qB	ÖB	��B	ĜB	ƨB	ǮB	ǮB	ȴB	ɺB	��B	ɺB	ƨB	��B	��B	�B	�B	�B	�
B	�
B	��B	��B	��B	�B	�#B	�#B	�/B	�HB	�NB	�HB	�BB	�BB	�BB	�HB	�ZB	�TB	�ZB	�mB	�sB	�sB	�mB	�mB	�fB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
%B
+B
+B
+B
+B
%B
B
B
+B
+B
+B
1B
+B
B
%B
1B
DB
JB
JB
DB

=B

=B

=B
JB
PB
PB
JB
PB
VB
VB
VB
JB
hB
hB
hB
oB
hB
bB
oB
bB
VB
hB
bB
uB
{B
�B
{B
uB
uB
�B
�B
�B
�B
�B
�B
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
!�B
"�B
#�B
$�B
$�B
$�B
%�B
&�B
'�B
)�B
)�B
)�B
)�B
(�B
(�B
)�B
,B
+B
+B
+B
+B
,B
/B
/B
.B
.B
-B
.B
.B
.B
.B
0!B
0!B
.B
/B
.B
1'B
49B
49B
33B
33B
49B
49B
49B
33B
49B
5?B
6FB
6FB
6FB
5?B
5?B
7LB
7LB
8RB
8RB
8RB
6FB
7LB
8RB
:^B
9XB
8RB
9XB
9XB
:^B
9XB
8RB
<jB
=qB
=qB
=qB
?}B
@�B
A�B
A�B
@�B
@�B
A�B
A�B
A�B
A�B
B�B
D�B
D�B
C�B
D�B
D�B
G�B
H�B
H�B
H�B
G�B
F�B
H�B
I�B
K�B
K�B
J�B
I�B
I�B
J�B
J�B
K�B
K�B
J�B
L�B
M�B
L�B
K�B
J�B
K�B
L�B
L�B
N�B
M�B
N�B
O�B
O�B
N�B
O�B
P�B
Q�B
P�B
P�B
P�B
Q�B
R�B
R�B
R�B
Q�B
Q�B
R�B
R�B
R�B
Q�B
P�B
S�B
S�B
T�B
S�B
S�B
T�B
S�B
T�B
T�B
VB
VB
T�B
T�B
VB
VB
VB
T�B
VB
VB
VB
W
B
VB
W
B
W
B
W
B
VB
VB
W
B
W
B
W
B
W
B
XB
XB
XB
XB
XB
W
B
XB
W
B
VB
W
B
XB
W
B
YB
ZB
[#B
\)B
\)B
\)B
\)B
\)B
[#B
[#B
ZB
XB
YB
XB
XB
XB
XB
[#B
[#B
\)B
\)B
\)B
[#B
\)B
\)B
[#B
\)B
[#B
ZB
\)B
\)B
[#B
^5B
]/B
\)B
]/B
]/B
`BB
`BB
`BB
^5B
]/B
`BB
`BB
bNB
cTB
cTB
bNB
bNB
cTB
cTB
cTB
cTB
bNB
cTB
e`B
gmB
ffB
gmB
gmB
ffB
ffB
e`B
ffB
gmB
gmB
gmB
gmB
gmB
iyB
iyB
iyB
hsB
hsB
hsB
iyB
iyB
jB
iyB
iyB
iyB
iyB
jB
iyB
l�B
l�B
k�B
m�B
n�B
n�B
n�B
m�B
m�B
m�B
n�B
n�B
m�B
o�B
o�B
p�B
p�B
q�11111111111111111111111111111111111111111111111111144111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�!B�!B�cB�AB�MB�?B�TB�`B�RB�rB�xB�B�qB�qB�B��B�iB�}B��B��BB��BªB��B��B��B��B��B�%B��B�EB�mBŢB�B�TB��B��B�B� B�DB��B��B��B��B��B��B�nB��B��B�B�mG�O�G�O�G�O�B��B��B�dB��B�7B��BiyBR:G�O�G�O�BB�B
�B�G�O�B
�1B
ΊB
�XB
��B
��B
��B
��B
��B
��G�O�B
LJB
TaB
Y�G�O�G�O�B
�B
�B
�B
�B
tB
gB	��B	��B	��B	��B	̘B	��B	�rB	�B	��B	��B	��B	��B	��B	�vB	�wB	��B	�NG�O�B	t�B	tTB	�KB	�tB	}�B	vzB	i�G�O�B	5tB	!�B	K)B	I�B	F�B	I7B	R�B	RB	OB	IB	C-B	9�G�O�B	3�B	9�B	6�B	/�B	/�B	$�B	'�B	%zB	!bB	�G�O�B�G�O�G�O�B�B�'G�O�B�FB�&B�'B�B�uB��B��B��BیBܒBԯB��B�MG�O�B�}B�B�DG�O�B�*B�/B��B�HB�pB�BcB��B�@B�bB��B��B�B�B��B�B�
B�B��B~wB��B��B��B� B��Bt�G�O�Ba�BuBpoBbhB[�BN�B_!Bh�Bh�Bf�Be�B`�B^�B[�BW�BP�BM�BK�BF�BB�BRTBM�G�O�B/ B;dBCGBCGBC-BCBCB7�G�O�B!bB,WB5�BC�B?�B6�B*B6�B1�G�O�B�B vB�B�B1B�B+kB2GB+�B�B%`B�B�B�B_B{B�BBVB
B,BBB�B
�G�O�B��B��B9B6B B�B6BBKB�B�B~BEBjB]B!HB�BeB%�B&2B!bB BB!HB'BOB;B# B;B"B 'B#�B�BaB�BkB�B�B%zB-CB(�B"�B"�B%�B(�B+�B-�B9�B<�B9�B7�B;�B<�B;�B5�B1'B7�B8�B3B0!B:�B:�B88B@BCGBH1BFYBTBT,BPHBK�BWYB[WB\]B]dB^jB_pB`\B_pB\xB[�B]dB]~BY�B_�B`�Bb�Bd�Bf�Bh�Bm�Bm�Bm)BpBy�By�BxBvBqABn}B|6BcB�SB�{B��B�PB��B��B��B��B��B��B��B��B�#B� B��B��B��B��B�]B�cB�hB�|B��B��B�rB��B��B��B��B��B��B��B�"B�B� B�B�2B�FB�9B�EB�YB�YB՛B�\B�nB�tB�B�tB�B��B�B��B��B��B��B��B�B�B�8B	 4B	B	1B	
#B	vB	\B	KB	�B	�B	�B	�B	$B	B	�B	#�B	# B	&2B	($B	+B	-B	2-B	6`B	3�B	;�B	@�B	AB	H�B	KB	H1B	SB	TB	TB	S&B	U2B	V9B	^5B	^�B	_�B	c�B	hsB	i�B	h�B	f�B	c�B	k�B	n�B	n�B	m�B	o�B	p�B	p�B	p�B	r�B	t9B	~(B	� B	�AB	�%B	�=B	�XB	�XB	�)B	�0B	�JB	�JB	�JB	�XB	�lB	�^B	�~B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��G�O�G�O�G�O�B	��B	�B	��B	�B	�
B	�
G�O�B	�B	�B	�"B	�5B	�-G�O�B	�[B	��B	�iB	�?B	�XB	�lB	��B	�xB	��B	��B	ðG�O�B	��B	ƨB	��B	��B	��B	��B	��B	�#G�O�B	�B	�B	�B	�B	�B	�$B	�$G�O�B	�FB	�2B	�KB	�=B	�=B	�dB	�HB	�4B	�bB	�\B	�\B	�vB	�bB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	�G�O�B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��G�O�G�O�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�G�O�B	�B	��B	�B	�B	�B	�B	�B	�B	�"B	�B	�(G�O�B	�(B
B
B
'B
B
'B
 B
;B
;B
3B
9B
%B
+B
+B
B
+B
?B
9B
9B
EB
EB
EB
1B
EG�O�B
YB
fB
^B
JB
JB
^B

rB

XB

rB
JB
PB
jB
~B
jB
pB
VB
pG�O�B
�B
�B
hB
oB
�B
}B
�G�O�G�O�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�G�O�B
�B
�B
�B
�B
�B
�B
�B
�B
�G�O�B
�B
�B
!�B
"�B
#�B
$�B
$�B
$�B
%�B
'B
(
B
*0B
)�B
*B
*0B
)B
)B
*B
,B
+B
+B
+B
+B
,B
/B
/B
.B
./B
-)B
./B
./B
./B
.IB
0;B
0;G�O�B
/OB
.cB
1AB
49B
4TB
33B
3MB
4TB
4TB
4TB
3�B
4TB
5ZB
6`B
6`B
6`B
5ZB
5ZB
7fB
7fB
8RB
8RB
8lG�O�B
7fB
8lB
:xB
9rB
8lB
9XB
9rB
:xB
9�B
8�B
<jB
=�B
=�B
=�B
?�B
@�B
A�B
A�B
@�B
@�B
A�B
A�B
A�B
A�B
B�B
D�B
D�B
C�B
D�B
D�B
G�B
H�B
H�B
H�B
G�B
F�B
H�B
I�B
K�B
K�B
J�B
I�B
I�B
J�B
J�B
K�B
K�B
J�B
L�B
M�B
L�B
K�B
KB
K�B
L�B
L�B
N�B
NB
N�B
O�B
O�B
N�B
O�B
P�B
RB
Q B
Q B
Q B
RB
S&B
R�B
R�B
RB
RB
SB
SB
R�B
RB
Q B
S�B
TB
T�B
TB
TB
T�B
TB
T�B
T�B
VB
VB
UB
T�B
VB
VB
VB
UB
U�B
VB
VB
W
B
VB
W
B
W
B
W
B
VB
VB
W$B
W$B
W$B
W$B
X+B
XB
W�B
X+B
X+B
W$B
XB
W$B
V9B
W$B
X+B
W?B
Y1B
Z7B
[=B
\)B
\CB
\)B
\B
\)B
[=B
[=B
Z7G�O�B
Y1B
X+B
X+B
XEB
XEB
[#B
[=B
\CB
\)B
\CB
[=B
\CB
\CB
[=B
\CB
[=B
ZQB
\CB
\CB
[qB
^OB
]/B
\]B
]IB
]/B
`\B
`\B
`\G�O�B
]dB
`\B
`\B
bNB
cTB
cnB
bNB
bhB
cnB
cTB
cnB
cnB
bhB
cnB
eFB
gmB
ffB
gmB
gmB
f�B
f�B
ezB
f�B
gmB
gmB
g�B
g�B
gmB
iyB
iyB
iyB
h�B
h�B
h�B
i�B
i�B
j�B
i�B
i�B
i�B
i�B
j�B
i�B
l�B
l�B
k�B
m�B
n�B
n�B
n�B
m�B
m�B
m�B
n�B
n�B
m�B
o�B
o�B
p�B
p�B
q�11111111111111111111111111111111111111111111111111144411111111441111411111111141114411111111111111111111111411111114111111111111411111111114144114111111111111141114111111111111111111111111114111111111111111111111141111111141111111114111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444111111411111411111111111411111111411111114111111111111111111111111111111141111111111111111114411111111111141111111111141111111111111111111111114111111111111111114111111144111111111111114111111111411111111111111111111111111111111111411111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�;o;o;o;o;o;o;o;o;oG�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
G�O�G�O�<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.01(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201803070032562018030700325620180307003256201806221238282018062212382820180622123828201804050435272018040504352720180405043527  JA  ARFMdecpA19c                                                                20180303093512  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180303003513  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180303003515  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180303003516  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180303003516  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180303003516  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180303003516  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcp2.8e                                                                20180303003516  QCF$                G�O�G�O�G�O�            4000JA  ARGQaqcp2.8e                                                                20180303003516  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180303003516  QCF$                G�O�G�O�G�O�            4000JA  ARGQrqcpt16c                                                                20180303003517  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180303003517                      G�O�G�O�G�O�                JA  ARUP                                                                        20180303005531                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180303153841  CV  JULD            G�O�G�O�F�                JM  ARSQJMQC2.0                                                                 20180304000000  CF  TEMP_ADJUSTED_QCC  C  G�O�                JM  ARSQJMQC2.0                                                                 20180305000000  CF  PSAL_ADJUSTED_QCB�  D�� G�O�                JM  ARCAJMQC2.0                                                                 20180306153256  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180306153256  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404193527  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622033828  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115121518                      G�O�G�O�G�O�                