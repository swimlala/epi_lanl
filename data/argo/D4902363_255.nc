CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-07-23T03:35:59Z creation;2018-07-23T03:36:03Z conversion to V3.1;2019-12-19T07:38:36Z update;     
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
_FillValue                 �  I8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M    PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p@   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t(   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �H   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �0   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �P   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ݈   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20180723033559  20200115131516  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0576_255                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @�nt���1   @�nt�}( @9��l�C��dFe��O1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   BffBffB��B��B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<fD<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU�fDV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�C3D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@�Q�A (�A (�A@(�A`(�A�{A�{A�{A�{A�{A�{A�{A�{B 
=Bp�Bp�B��B��B(
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
�C�C�C�C�C�C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HD  �D ��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D	 �D	��D
 �D
��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D  �D ��D! �D!��D" �D"��D# �D#��D$ �D$��D% �D%��D& �D&��D' �D'��D( �D(��D) �D)��D* �D*��D+ �D+��D, �D,��D- �D-��D. �D.��D/ �D/��D0 �D0��D1 �D1��D2 �D2��D3 �D3��D4 �D4��D5 �D5��D6 �D6��D7 �D7��D8 �D8��D9 �D9��D: �D:��D; �D;��D<
D<��D= �D=��D> �D>��D? �D?��D@ �D@��DA �DA��DB �DB��DC �DC��DD �DD��DE �DE��DF �DF��DG �DG��DH �DH��DI �DI��DJ �DJ��DK �DK��DL �DL��DM �DM��DN �DN��DO �DO��DP �DP��DQ �DQ��DR �DR��DS �DS��DT �DT��DU �DU�
DV �DV��DW �DW��DX �DX��DY �DY��DZ �DZ��D[ �D[��D\ �D\��D] �D]��D^ �D^��D_ �D_��D` �D`��Da �Da��Db �Db��Dc �Dc��Dd �Dd��De �De��Df �Df��Dg �Dg��Dh �Dh��Di �Di��Dj �Dj��Dk �Dk��Dl �Dl��Dm �Dm��Dn �Dn��Do �Do��Dp �Dp��Dq �Dq��Dr �Dr��Ds �Ds��Dt �Dt��Du �Du��Dv �Dv��Dw �Dw��Dx �Dx��Dy �Dy��Dz �Dz��D{ �D{��D| �D|��D} �D}��D~ �D~��D �D��D� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD��D�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�=D��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RDRD��RD� RD�@RDÀRD��RD� RD�@RDĀRD��RD� RD�@RDŀRD��RD� RD�@RDƀRD��RD� RD�@RDǀRD��RD� RD�@RDȀRD��RD� RD�@RDɀRD��RD� RD�@RDʀRD��RD� RD�@RDˀRD��RD� RD�@RD̀RD��RD� RD�@RD̀RD��RD� RD�@RD΀RD��RD� RD�@RDπRD��RD� RD�@RDЀRD��RD� RD�@RDрRD��RD� RD�@RDҀRD��RD� RD�@RDӀRD��RD� RD�@RDԀRD��RD� RD�@RDՀRD��RD� RD�@RDրRD��RD� RD�@RD׀RD��RD� RD�@RD؀RD��RD� RD�@RDـRD��RD� RD�@RDڀRD��RD� RD�@RDۀRD��RD� RD�@RD܀RD��RD� RD�@RD݀RD��RD� RD�@RDހRD��RD� RD�@RD߀RD��RD� RD�@RD��RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�C�D�RD��RD� RD�@RD�RD��RD� RD�@RD��RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 AΩ�AΣ�AΛ�AΡ�AΗ�AΕ�AΕ�AΗ�A΁A�A�AͬA��A�=qA�`BA�\)AɶFA�9XA��TA��mA��A�ȴA��;A��A�1A���A�p�A��HA�bNA�dZA��A�A���A��A� �A�S�A�I�A�=qA��^A��+A�hsA��A���A��RA��
A�ZA���A�A�A���A�`BA���A���A�XA��A�z�A�33A���A���A�
=A���A�S�A��hA�=qA���A���A�ffA�Q�A�I�A�t�A���A��A��A��A�bNA�(�A���A���A�C�A�x�A�S�A�JA�ƨA��jA�t�A�+A��A��hA�"�A�`BA�ƨA�5?A�;dAK�A~��A}"�A|�DA|-Ay|�Aw��AwC�Av��Av=qAu�^AtbNAr�RAp(�An�+Am�;Al�jAj�uAh�/Ag\)Ae�Ad�yAd=qAb�9Aa�-A`r�A_ƨA^��A]�TA\��AZ�HAY��AX��AV�yAU��AT�AS"�AQ��AQ33AQ%AP�jAP��AO�ANZAM\)ALz�AK�AI��AH�/AHjAG�^AF�AF��AF��AE�-ADȴAC��ACO�AC%AB��AB(�A@r�A?�7A>ȴA=�;A<�yA<ffA;��A:~�A8�RA7��A5�
A4^5A3�-A3K�A2A�A1&�A0�A/�A/oA.�\A.(�A-��A-��A-p�A,��A,A�A+\)A*$�A)�TA)\)A)O�A)�A(n�A'
=A&-A%p�A$��A$=qA#�hA"��A!�A!�hA ĜA�PA��AE�A�Ax�A�uAQ�A(�AA�hA��Ax�A��Az�A��A�#A&�AȴA�TAĜA9XA��A��AE�AhsA��A �A�TA��A�AA�A+A
n�A	�A�7A�9A��AC�A�TA��A~�A�Ao@�ƨ@�~�@���@��u@�"�@���@��`@�33@��h@�ȴ@���@�(�@��@��@�p�@�j@�F@�j@�v�@��`@��m@�S�@�\@�ȴ@�&�@��/@ە�@�`B@�Z@�dZ@�@֗�@թ�@�(�@�@�x�@мj@�I�@Ͼw@ϥ�@ύP@�33@�v�@���@�  @�l�@���@ə�@�Ĝ@�;d@�M�@���@�9X@� �@�l�@���@�%@�S�@�J@���@�bN@��w@�+@��H@�~�@�J@�&�@��@�Ĝ@�z�@�Z@�1@���@�S�@�V@�%@�33@�X@� �@���@�ȴ@��\@�~�@�=q@�@�`B@��j@��
@�^5@���@��H@���@�7L@� �@�l�@�"�@��\@��+@��+@��\@��\@��\@��\@��+@�~�@�ff@�M�@��@���@�`B@��`@�Ĝ@��@�1@�ȴ@�=q@�?}@��/@�I�@��P@��@�M�@��^@���@� �@�
=@��+@�n�@�-@��@��T@��#@���@��h@��@���@���@��R@��!@��\@�v�@���@��^@�p�@�?}@���@��j@�bN@�(�@��w@�S�@���@�v�@�=q@���@���@�p�@�?}@�%@��/@���@�bN@�9X@�1'@�1'@�(�@��@��
@��@��@��y@��!@�~�@�5?@���@��h@�Ĝ@�1'@��
@��F@��P@�l�@�
=@��!@�-@�p�@�/@��@���@��@�r�@�Z@�I�@�1'@��@��@��@�dZ@�@���@�n�@�V@�5?@�{@�@��T@���@��@��@��@��/@���@��@�r�@�1'@��@K�@~�@~ff@~@}�T@}��@}��@}p�@}�@|��@|�@|�j@|j@|(�@{�m@{�F@{S�@{33@{"�@z�H@z�\@y��@y7L@xĜ@x�u@w�@wK�@w�@vE�@u@u/@t�j@tI�@s��@s@r�H@r�@r�@r�H@r�!@rJ@qhs@q�@p��@p��@pQ�@o�w@o�P@o|�@o\)@o�@n�y@n�+@n$�@m��@m�h@l�/@l9X@kƨ@kC�@k@j��@j^5@j�@i�^@iG�@h��@h�9@h�u@hr�@hA�@g�@g�P@g\)@gK�@g+@f�@f{@e�T@ep�@d�/@d��@dj@dj@d1@c�@b��@b�!@b��@b�\@b^5@a�#@a%@_�@_\)@^��@]�@]�@\��@\��@\I�@[�F@[dZ@[C�@[@ZM�@Z�@Y�@Y��@Yx�@YG�@X��@X�9@XbN@X  @W\)@W
=@VE�@U�@T�j@TZ@T1@S�F@St�@SS�@S33@R�@R�H@R�!@R�\@R~�@RM�@R�@Q�@Q�@Q�@Q�#@Q��@Q��@Q��@Q��@Qhs@Q7L@Q�@P��@PĜ@P�@PbN@PQ�@O�@N��@M��@M?}@L�@L�@L(�@K��@K33@K@J�@J~�@J=q@I�^@I7L@I%@H��@H�9@H�9@H�@HQ�@H1'@H  @G�;@F�y@FV@F@E�T@E�T@E��@D�@D�j@D�D@Dj@DZ@DI�@D1@C�m@C�@C@B�\@B^5@B�@BJ@BJ@A�@A��@A�7@A7L@A%@@bN@?�w@?��@?l�@?�@>�@>��@>$�@=�@=�T@=@=@=�@=?}@<�@<�/@<��@<I�@;�m@;��@;t�@;dZ@;C�@:�\@9��@9hs@8�`@8��@81'@7�@7|�@7K�@7�@6�R@6V@6@5@5�h@5p�@5?}@5�@4��@4�j@4��@4�D@4Z@4(�@3��@3�F@3�@3t�@3o@2n�@1��@17L@0��@/�@/l�@/K�@/�@.�R@.ff@.5?@.@-��@-�@-`B@,��@,�j@,��@,Z@,(�@,1@+��@+��@+dZ@+S�@+C�@+@*��@*��@*~�@*n�@*^5@*�@)hs@)7L@)�@)�@)�@(�`@(A�@(  @'�@'�@'l�@';d@'�@&�@&�+@&$�@%��@%@%�-@%�@$�@$�/@$��@$�j@$�j@$�@$�@$��@$Z@$�@#��@#dZ@#33@#"�@"��@"��@"��@"��@"~�@"n�@!�#@!��@!hs@!7L@!7L@ ��@ ��@ �@ r�@ Q�@   @�w@�P@l�@\)@K�@��@�+@ff@V@ff@ff@ff@ff@ff@ff@V@�@O�@�@��@�j@�@z�@Z@I�@9X@1@�@�H@~�@n�@^5@^5@M�@=q@=q@J@��@�7@G�@&�@%@��@�9@bN@  @�P@\)@;d@
=@��@ȴ@�+@ff@$�@�@��@��@�@p�@O�@?}@�/@�j@�@��@z�@9X@�@1@ƨ@t�@C�@@��@��@n�@^5@J@�@�^@x�@G�@�@��@bN@bN@bN@Q�@A�@1'@ �@ �@b@�;@�@|�@�@�R@@�h@`B@O�@O�@O�@?}@/@V@��@��@��@��@��@��@��@��@��@�@�@��@z�@Z@�@�
@�F@��@t�@S�@C�@C�@"�@
�H@
��@
��@
�!@
�!@
�\@
~�@
^5@
=q@
�@
J@	��@	x�@	x�@	x�@	x�@	G�@��@��@�`@Ĝ@�u@A�@A�@ �@  @  @��@�w@�@��@\)@+@��@��@��@�@�-@�@O�@?}@?}@V@V@�/@��@��@�@z�@9X@1@��@�
@�F@��@�@"�@@�@�H@�H@��@��@~�@=q@�@��@�#@��@hs@%@ �`@ �9@ ��@ �u@ Q�@  �@  �111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 AΩ�AΣ�AΛ�AΡ�AΗ�AΕ�AΕ�AΗ�A΁A�A�AͬA��A�=qA�`BA�\)AɶFA�9XA��TA��mA��A�ȴA��;A��A�1A���A�p�A��HA�bNA�dZA��A�A���A��A� �A�S�A�I�A�=qA��^A��+A�hsA��A���A��RA��
A�ZA���A�A�A���A�`BA���A���A�XA��A�z�A�33A���A���A�
=A���A�S�A��hA�=qA���A���A�ffA�Q�A�I�A�t�A���A��A��A��A�bNA�(�A���A���A�C�A�x�A�S�A�JA�ƨA��jA�t�A�+A��A��hA�"�A�`BA�ƨA�5?A�;dAK�A~��A}"�A|�DA|-Ay|�Aw��AwC�Av��Av=qAu�^AtbNAr�RAp(�An�+Am�;Al�jAj�uAh�/Ag\)Ae�Ad�yAd=qAb�9Aa�-A`r�A_ƨA^��A]�TA\��AZ�HAY��AX��AV�yAU��AT�AS"�AQ��AQ33AQ%AP�jAP��AO�ANZAM\)ALz�AK�AI��AH�/AHjAG�^AF�AF��AF��AE�-ADȴAC��ACO�AC%AB��AB(�A@r�A?�7A>ȴA=�;A<�yA<ffA;��A:~�A8�RA7��A5�
A4^5A3�-A3K�A2A�A1&�A0�A/�A/oA.�\A.(�A-��A-��A-p�A,��A,A�A+\)A*$�A)�TA)\)A)O�A)�A(n�A'
=A&-A%p�A$��A$=qA#�hA"��A!�A!�hA ĜA�PA��AE�A�Ax�A�uAQ�A(�AA�hA��Ax�A��Az�A��A�#A&�AȴA�TAĜA9XA��A��AE�AhsA��A �A�TA��A�AA�A+A
n�A	�A�7A�9A��AC�A�TA��A~�A�Ao@�ƨ@�~�@���@��u@�"�@���@��`@�33@��h@�ȴ@���@�(�@��@��@�p�@�j@�F@�j@�v�@��`@��m@�S�@�\@�ȴ@�&�@��/@ە�@�`B@�Z@�dZ@�@֗�@թ�@�(�@�@�x�@мj@�I�@Ͼw@ϥ�@ύP@�33@�v�@���@�  @�l�@���@ə�@�Ĝ@�;d@�M�@���@�9X@� �@�l�@���@�%@�S�@�J@���@�bN@��w@�+@��H@�~�@�J@�&�@��@�Ĝ@�z�@�Z@�1@���@�S�@�V@�%@�33@�X@� �@���@�ȴ@��\@�~�@�=q@�@�`B@��j@��
@�^5@���@��H@���@�7L@� �@�l�@�"�@��\@��+@��+@��\@��\@��\@��\@��+@�~�@�ff@�M�@��@���@�`B@��`@�Ĝ@��@�1@�ȴ@�=q@�?}@��/@�I�@��P@��@�M�@��^@���@� �@�
=@��+@�n�@�-@��@��T@��#@���@��h@��@���@���@��R@��!@��\@�v�@���@��^@�p�@�?}@���@��j@�bN@�(�@��w@�S�@���@�v�@�=q@���@���@�p�@�?}@�%@��/@���@�bN@�9X@�1'@�1'@�(�@��@��
@��@��@��y@��!@�~�@�5?@���@��h@�Ĝ@�1'@��
@��F@��P@�l�@�
=@��!@�-@�p�@�/@��@���@��@�r�@�Z@�I�@�1'@��@��@��@�dZ@�@���@�n�@�V@�5?@�{@�@��T@���@��@��@��@��/@���@��@�r�@�1'@��@K�@~�@~ff@~@}�T@}��@}��@}p�@}�@|��@|�@|�j@|j@|(�@{�m@{�F@{S�@{33@{"�@z�H@z�\@y��@y7L@xĜ@x�u@w�@wK�@w�@vE�@u@u/@t�j@tI�@s��@s@r�H@r�@r�@r�H@r�!@rJ@qhs@q�@p��@p��@pQ�@o�w@o�P@o|�@o\)@o�@n�y@n�+@n$�@m��@m�h@l�/@l9X@kƨ@kC�@k@j��@j^5@j�@i�^@iG�@h��@h�9@h�u@hr�@hA�@g�@g�P@g\)@gK�@g+@f�@f{@e�T@ep�@d�/@d��@dj@dj@d1@c�@b��@b�!@b��@b�\@b^5@a�#@a%@_�@_\)@^��@]�@]�@\��@\��@\I�@[�F@[dZ@[C�@[@ZM�@Z�@Y�@Y��@Yx�@YG�@X��@X�9@XbN@X  @W\)@W
=@VE�@U�@T�j@TZ@T1@S�F@St�@SS�@S33@R�@R�H@R�!@R�\@R~�@RM�@R�@Q�@Q�@Q�@Q�#@Q��@Q��@Q��@Q��@Qhs@Q7L@Q�@P��@PĜ@P�@PbN@PQ�@O�@N��@M��@M?}@L�@L�@L(�@K��@K33@K@J�@J~�@J=q@I�^@I7L@I%@H��@H�9@H�9@H�@HQ�@H1'@H  @G�;@F�y@FV@F@E�T@E�T@E��@D�@D�j@D�D@Dj@DZ@DI�@D1@C�m@C�@C@B�\@B^5@B�@BJ@BJ@A�@A��@A�7@A7L@A%@@bN@?�w@?��@?l�@?�@>�@>��@>$�@=�@=�T@=@=@=�@=?}@<�@<�/@<��@<I�@;�m@;��@;t�@;dZ@;C�@:�\@9��@9hs@8�`@8��@81'@7�@7|�@7K�@7�@6�R@6V@6@5@5�h@5p�@5?}@5�@4��@4�j@4��@4�D@4Z@4(�@3��@3�F@3�@3t�@3o@2n�@1��@17L@0��@/�@/l�@/K�@/�@.�R@.ff@.5?@.@-��@-�@-`B@,��@,�j@,��@,Z@,(�@,1@+��@+��@+dZ@+S�@+C�@+@*��@*��@*~�@*n�@*^5@*�@)hs@)7L@)�@)�@)�@(�`@(A�@(  @'�@'�@'l�@';d@'�@&�@&�+@&$�@%��@%@%�-@%�@$�@$�/@$��@$�j@$�j@$�@$�@$��@$Z@$�@#��@#dZ@#33@#"�@"��@"��@"��@"��@"~�@"n�@!�#@!��@!hs@!7L@!7L@ ��@ ��@ �@ r�@ Q�@   @�w@�P@l�@\)@K�@��@�+@ff@V@ff@ff@ff@ff@ff@ff@V@�@O�@�@��@�j@�@z�@Z@I�@9X@1@�@�H@~�@n�@^5@^5@M�@=q@=q@J@��@�7@G�@&�@%@��@�9@bN@  @�P@\)@;d@
=@��@ȴ@�+@ff@$�@�@��@��@�@p�@O�@?}@�/@�j@�@��@z�@9X@�@1@ƨ@t�@C�@@��@��@n�@^5@J@�@�^@x�@G�@�@��@bN@bN@bN@Q�@A�@1'@ �@ �@b@�;@�@|�@�@�R@@�h@`B@O�@O�@O�@?}@/@V@��@��@��@��@��@��@��@��@��@�@�@��@z�@Z@�@�
@�F@��@t�@S�@C�@C�@"�@
�H@
��@
��@
�!@
�!@
�\@
~�@
^5@
=q@
�@
J@	��@	x�@	x�@	x�@	x�@	G�@��@��@�`@Ĝ@�u@A�@A�@ �@  @  @��@�w@�@��@\)@+@��@��@��@�@�-@�@O�@?}@?}@V@V@�/@��@��@�@z�@9X@1@��@�
@�F@��@�@"�@@�@�H@�H@��@��@~�@=q@�@��@�#@��@hs@%@ �`@ �9@ ��@ �u@ Q�@  �@  �111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B+B+B,B,B,B,B)�B&�B�BuBJB\BhB�B,B:^B33B�B�;B�LBBt�B� BdZBhsB��B�=BgmB�=BgmB��B�DB�DB�B~�Bm�B}�B�Bz�BgmBH�BK�BJ�BQ�BL�BA�B7LB6FB!�B	7B��B��B  B	7B��B�B�/B�BBÖB�dB��B�}B�\BcTBgmBL�B-B33B/B$�B%B
��BDBB
��B
��B
�)B
�fB
�/B
��B
�?B
��B
�!B
�B
��B
�uB
�B
t�B
r�B
T�B
[#B
]/B
J�B
J�B
E�B
"�B
!�B
<jB
6FB
0!B
%�B
hB	��B	�B	�sB	�B	�ZB	B	ǮB	�dB	�FB	�XB	�FB	��B	��B	��B	��B	�PB	�B	w�B	dZB	n�B	_;B	O�B	S�B	D�B	K�B	D�B	M�B	VB	N�B	K�B	<jB	!�B	(�B	"�B	�B	JB	�B	�B	�B	hB	�B	�B	+B	B��B	+B	B��B��B�NB�NB�sB�BB�5B�#B�B��B�LB�XB��B�B�dB�qB�B��B�'B��B��B�B�B�B�B�B��B��B��B�=B��B��B��B��B�1Bw�B}�B� B}�Bz�Bu�Bk�Bq�Bp�BgmBYBe`Bk�Bl�BgmB`BBk�Bl�BgmB^5BM�BK�BQ�BS�BH�B33BD�BK�BB�B6FBF�BA�B>wB@�B<jB<jBD�BG�BH�B@�B7LB)�B(�B'�B�B+B �B�B�B{BbB-B$�B�B{B�B"�B�B�B�B�BhBPBBbB�B�B�B�B�BVBB%BoB�B�B{B��BbB#�B�BbB�B$�B(�B&�B�B�BoB.B)�B.B.B33B1'B-B&�B$�B�B'�B(�B!�B$�B �B&�B%�B/B49B-B+B!�B�B(�B/B5?B7LB8RB=qB;dB:^B8RB@�BB�BA�BA�B>wB;dB;dB1'B.B-B0!B?}BI�BJ�BQ�BS�BP�BN�BM�BJ�BD�BA�B7LBXBXB]/B\)BhsBn�Bn�Bv�Bw�Bw�Bw�Bw�Bw�Bv�Bv�Bt�Bs�Bq�Br�Bt�Bs�Bv�Bt�Bm�BhsBv�Bs�B|�B}�B|�B�B�B�%B�%B�1B�JB��B��B��B��B��B��B��B��B��B��B��B�'B�9B�3B�3B�!B�FB�LB�dB�jB�wB�qB�}B�wB��BÖBĜB��B��B��B��B��B��B��B��B�
B�B�/B�)B�)B�#B�B�#B�#B�B�HB�ZB�TB�`B�TB�TB�B�B��B��B��B��B��B��B	  B	JB	bB	hB	oB	�B	�B	�B	�B	�B	�B	�B	�B	 �B	%�B	)�B	.B	.B	/B	2-B	2-B	2-B	7LB	7LB	>wB	B�B	B�B	A�B	F�B	E�B	F�B	J�B	N�B	Q�B	T�B	YB	ZB	ZB	YB	ZB	]/B	]/B	]/B	^5B	_;B	aHB	cTB	dZB	gmB	iyB	iyB	iyB	iyB	k�B	o�B	q�B	q�B	t�B	x�B	w�B	z�B	}�B	� B	�B	�B	�1B	�DB	�JB	�JB	�DB	�=B	�7B	�DB	�bB	�uB	�uB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�-B	�-B	�3B	�?B	�FB	�RB	�RB	�RB	�RB	�XB	�dB	�qB	�jB	�dB	�dB	B	��B	ÖB	ƨB	ǮB	ȴB	ƨB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�#B	�;B	�BB	�BB	�BB	�TB	�ZB	�TB	�NB	�mB	�mB	�mB	�sB	�sB	�mB	�sB	�sB	�sB	�mB	�yB	�mB	�sB	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
  B
B
B
%B
%B
B
B
B
%B
	7B

=B
DB
DB
DB
DB
DB

=B

=B
+B
DB
VB
\B
bB
VB
PB
hB
hB
uB
uB
oB
hB
hB
hB
hB
oB
�B
�B
�B
�B
�B
�B
�B
{B
�B
{B
�B
�B
�B
�B
�B
�B
�B
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
!�B
 �B
�B
�B
�B
�B
!�B
#�B
"�B
#�B
&�B
&�B
&�B
%�B
&�B
'�B
)�B
)�B
+B
+B
+B
+B
+B
,B
,B
,B
,B
,B
,B
,B
-B
)�B
(�B
'�B
,B
,B
,B
/B
2-B
2-B
1'B
2-B
33B
49B
33B
33B
49B
49B
5?B
6FB
6FB
7LB
7LB
8RB
7LB
7LB
9XB
9XB
8RB
8RB
9XB
8RB
9XB
9XB
8RB
6FB
;dB
<jB
<jB
;dB
:^B
9XB
;dB
=qB
<jB
<jB
=qB
=qB
<jB
<jB
=qB
=qB
?}B
?}B
=qB
@�B
A�B
A�B
B�B
A�B
A�B
A�B
A�B
?}B
?}B
?}B
B�B
B�B
C�B
B�B
D�B
D�B
D�B
C�B
C�B
B�B
C�B
E�B
E�B
F�B
E�B
E�B
F�B
G�B
F�B
F�B
F�B
H�B
H�B
H�B
H�B
G�B
G�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
J�B
I�B
F�B
J�B
K�B
K�B
L�B
L�B
L�B
L�B
M�B
L�B
L�B
J�B
J�B
M�B
O�B
P�B
P�B
P�B
P�B
P�B
O�B
N�B
N�B
O�B
P�B
P�B
P�B
O�B
O�B
O�B
P�B
R�B
S�B
S�B
T�B
S�B
S�B
T�B
T�B
T�B
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
VB
XB
XB
XB
XB
XB
XB
XB
XB
W
B
XB
YB
YB
ZB
ZB
[#B
ZB
[#B
[#B
[#B
[#B
[#B
[#B
]/B
_;B
_;B
_;B
^5B
_;B
_;B
^5B
^5B
]/B
]/B
]/B
\)B
\)B
\)B
_;B
bNB
cTB
cTB
cTB
cTB
bNB
cTB
cTB
cTB
dZB
dZB
dZB
dZB
dZB
dZB
cTB
cTB
cTB
bNB
bNB
cTB
cTB
cTB
e`B
e`B
e`B
e`B
ffB
ffB
e`B
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
e`B
gmB
hsB
hsB
hsB
gmB
gmB
hsB
iyB
hsB
gmB
gmB
iyB
iyB
iyB
iyB
iyB
iyB
jB
iyB
hsB
iyB
iyB
jB
hsB
gmB
jB
l�B
l�B
m�B
m�B
m�B
m�B
n�B
o�B
p�B
o�B
o�B
o�B
p�B
q�B
q�B
q�B
q�B
q�B
p�B
r�B
s�B
s�B
s�B
s�B
r�B
r�B
r�B
r�B
s�B
s�B
r�B
r�B
r�B
s�B
s�B
t�B
t�B
t�B
t�B
u�B
v�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B+B+B,B,"B,B,B*B'8BpB�B�B BuB�B-�B<6B6�B �B��B�oB
=BvB��Bi�Bl�B�B��Bl"B��Bl"B�EB��B�jB�B� Bo�B~�B��B{Bh�BK�BM�BL�BR�BM�BC-B8�B72B#�B�B��B�rB B	�B�dB�hB�pB�bB�tB�"B͹B��B��Bg8BjBO�B1B4�B0�B&LB	B OB�B�B
��B
��B
�B
��B
�B
��B
��B
�_B
��B
��B
��B
��B
��B
v`B
tB
WsB
\xB
^OB
L~B
K�B
F�B
%�B
#�B
<�B
7B
0�B
&�B
@B	�PB	�iB	�KB	�B	��B	ňB	ɠB	�VB	�8B	�^B	�2B	��B	� B	�$B	�kB	��B	�mB	y�B	f�B	o�B	`�B	R B	UMB	F�B	MB	F%B	NVB	V9B	O\B	L0B	=�B	#�B	*0B	$&B	;B	�B	xB	 \B	B	TB	�B	
B	�B	GB�PB	�B	�B��B��B�B�B�yB�|B�pB�B�?B̈́B��B��B�qB��B�PB�(B��B�yB��B�B�B��B��B��B�cB�kB��B��B��B��B��B�KB�B�B�RBy�BB�B~�B{�Bv�Bl�Br�Bq[Bh�BZ�Bf�BlBmBh>BaHBk�Bl�Bg�B_BOBBMPBR�BT�BJ	B5�BE�BLdBC�B7�BG_BB�B?�BAUB=�B=�BE9BHBIBA B8B+�B*0B)BB
	B!�B�B�B9B�B-�B%�B
BB�B#�B�B�B�BWB�B�B�BhB=BeB=B5B=B\BB�B[B]BIBgBoBNB$&B�B�B vB%�B)_B'mB�B�B�B.B*�B.}B.}B3MB1[B-wB'�B%�BB(sB)�B"�B%�B!�B'�B&�B/�B4nB-�B+�B# B!B)�B/�B5�B7�B8�B=�B;�B:�B9	B@�BB�BA�BA�B>�B;�B;�B2B/5B.cB1vB@OBJ#BK^BR BT,BQ4BOBBN<BKDBE�BB�B9XBX_BX�B]�B\�Bh�Bn�Bo Bv�Bw�Bw�Bw�Bw�Bw�Bv�Bv�Bt�Bs�BrBr�Bt�BtBv�Bt�Bn/BiDBw2Bt�B}"B~wB}�B��B��B��B��B��B�B��B��B��B�B��B��B�B�B�=B��B�_B�[B�TB�hB�hB��B�zB��B��B��B��B��B��B��B��B��B�B��B�B�B�B� B�&B�,B�2B�?B�KB�IB�CB�CB�=B�eB�WB�=BٚB�bB�B�B�B�B��B��B�B��B��B��B�8B�DB�<B	 �B	~B	}B	�B	�B	�B	�B	�B	�B	�B	�B	�B	B	 �B	&2B	*0B	./B	.IB	/5B	2-B	2GB	2|B	7fB	7�B	>�B	B�B	B�B	A�B	F�B	E�B	F�B	J�B	OB	R B	U2B	Y1B	Z7B	Z7B	YKB	ZQB	]IB	]IB	]IB	^OB	_;B	abB	cnB	d�B	g�B	i�B	i�B	i�B	i�B	k�B	o�B	q�B	q�B	uB	x�B	xB	z�B	~(B	�4B	�AB	�aB	�fB	�DB	�JB	�JB	�^B	�rB	��B	��B	�}B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�*B	�CB	�IB	�AB	�GB	�aB	�MB	�ZB	�`B	�lB	�lB	�lB	��B	��B	�B	��B	��B	��B	��B	ªB	��B	��B	��B	ǮB	��B	��B	��B	�	B	��B	��B	� B	��B	�(B	�BB	�HB	�9B	�SB	�_B	�qB	�VB	�\B	�vB	�vB	�TB	�tB	�B	�B	�mB	�B	�B	�B	�sB	�B	�B	�B	�B	�B	�B	�B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	�B	�%B	�$B	�B
 B
 B
 4B
;B
GB
?B
?B
MB
B
SB
YB
	lB

XB
^B
DB
^B
^B
^B

=B

XB
zB
xB
pB
vB
}B
�B
�B
�B
�B
�B
uB
�B
hB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
 �B
�B
�B
�B
�B
!�B
#�B
#B
$B
'B
&�B
'B
&B
'B
(
B
*B
*B
+B
+B
+B
+B
+B
,"B
,B
,"B
,"B
,"B
,"B
,"B
-)B
*0B
)*B
(>B
,=B
,"B
,WB
/OB
2GB
2GB
1[B
2GB
3MB
4TB
3MB
3MB
4TB
4nB
5ZB
6FB
6`B
7fB
7fB
8lB
7fB
7fB
9XB
9rB
8lB
8lB
9XB
8lB
9XB
9rB
8lB
6�B
;dB
<jB
<�B
;B
:xB
9�B
;B
=qB
<�B
<jB
=�B
=�B
<�B
<�B
=�B
=�B
?}B
?�B
=�B
@�B
A�B
A�B
B�B
A�B
A�B
A�B
A�B
?�B
?�B
?�B
B�B
B�B
C�B
B�B
D�B
D�B
D�B
C�B
C�B
B�B
C�B
E�B
E�B
F�B
E�B
E�B
F�B
G�B
F�B
F�B
F�B
H�B
H�B
H�B
H�B
G�B
G�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
J�B
I�B
F�B
J�B
K�B
K�B
L�B
L�B
L�B
L�B
M�B
L�B
L�B
J�B
KB
M�B
O�B
P�B
P�B
P�B
P�B
P�B
O�B
N�B
N�B
PB
Q B
Q B
Q B
O�B
O�B
PB
QB
R�B
TB
S�B
T�B
TB
TB
T�B
UB
UB
VB
V9B
W$B
W$B
W$B
W$B
VB
XB
XB
X+B
X+B
X+B
X+B
X+B
X+B
W
B
X+B
YB
Y1B
Z7B
Z7B
[#B
ZB
[=B
[=B
[=B
[=B
[=B
[WB
]IB
_VB
_;B
_;B
^5B
_;B
_;B
^B
^5B
]IB
]IB
]IB
\]B
\]B
\�B
_VB
bNB
cTB
cTB
cTB
cTB
bhB
cTB
cTB
cTB
dZB
d@B
dZB
dZB
dZB
dZB
cTB
cTB
c:B
bNB
bhB
cnB
cnB
cnB
ezB
ezB
ezB
ezB
fLB
ffB
ezB
ezB
ffB
fLB
ffB
ffB
f�B
ffB
ffB
f�B
f�B
f�B
e�B
gmB
hsB
hsB
hsB
g�B
g�B
hsB
iyB
hsB
g�B
g�B
iyB
i�B
i�B
iyB
i�B
i�B
jB
i�B
h�B
i�B
i�B
jB
h�B
g�B
j�B
l�B
l�B
mwB
m�B
m�B
m�B
n�B
o�B
p�B
o�B
o�B
o�B
p�B
q�B
q�B
q�B
q�B
q�B
p�B
r�B
s�B
s�B
s�B
s�B
r�B
r�B
r�B
r�B
s�B
s�B
r�B
r�B
r�B
s�B
s�B
t�B
t�B
t�B
t�B
u�B
v�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.01(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201807050035112018070500351120180705003511201807050200142018070502001420180705020014201807060026172018070600261720180706002617  JA  ARFMdecpA19c                                                                20180723123526  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180723033559  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180723033602  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180723033602  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180723033603  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180723033603  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180723033603  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180723033603  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180723033603  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180723033603                      G�O�G�O�G�O�                JA  ARUP                                                                        20180723040124                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180701153600  CV  JULD            G�O�G�O�F�s�                JM  ARCAJMQC2.0                                                                 20180704153511  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180704153511  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180704170014  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180705152617  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115131516                      G�O�G�O�G�O�                