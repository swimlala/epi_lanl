CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2017-10-10T00:35:12Z creation;2017-10-10T00:35:16Z conversion to V3.1;2019-12-19T07:59:40Z update;     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20171010003512  20200115121517  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0576_167                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @�,u&N �1   @�,u�$�@:��g���d���+1   GPS     A   B   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�  @�  A   AffA@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BHffBP  BX  B`  Bg33Bp  Bx  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"fD"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D���D�<�D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�Q�@�Q�A (�A�\A@(�A`(�A�{A�{A�{A�{A�{A�{A�{A�{B 
=B
=B
=B
=B 
=B(
=B0
=B8
=B@
=BHp�BP
=BX
=B`
=Bg=qBp
=Bx
=B�B�B�8RB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�C �C�C�C�C�C
�C�C�C�C�C�C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC��{C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC��{C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HD  �D ��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D	 �D	��D
 �D
��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D  �D ��D! �D!��D"
D"��D# �D#��D$ �D$��D% �D%��D& �D&��D' �D'��D( �D(��D) �D)��D* �D*��D+ �D+��D, �D,��D- �D-��D. �D.��D/ �D/��D0 �D0��D1 �D1��D2 �D2��D3 �D3��D4 �D4��D5 �D5��D6 �D6��D7 �D7��D8 �D8��D9 �D9��D: �D:��D; �D;��D< �D<��D= �D=��D> �D>��D? �D?��D@ �D@��DA �DA��DB �DB��DC �DC��DD �DD��DE �DE��DF �DF��DG �DG��DH �DH��DI �DI��DJ �DJ��DK �DK��DL �DL��DM �DM��DN �DN��DO �DO��DP �DP��DQ �DQ��DR �DR��DS �DS��DT �DT��DU �DU��DV �DV��DW �DW��DX �DX��DY �DY��DZ �DZ��D[ �D[��D\ �D\��D] �D]��D^ �D^��D_ �D_��D` �D`��Da �Da��Db �Db��Dc �Dc��Dd �Dd��De �De��Df �Df��Dg �Dg��Dh �Dh��Di �Di��Dj �Dj��Dk �Dk��Dl �Dl��Dm �Dm��Dn �Dn��Do �Do��Dp �Dp��Dq �Dq��Dr �Dr��Ds �Ds��Dt �Dt��Du �Du��Dv �Dv��Dw �Dw��Dx �Dx��Dy �Dy��Dz �Dz��D{ �D{��D| �D|��D} �D}��D~ �D~��D �D��D� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�=D��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD�}D��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD���D��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RDRD��RD� RD�@RDÀRD��RD� RD�@RDĀRD��RD� RD�@RDŀRD��RD� RD�@RDƀRD��RD� RD�@RDǀRD��RD� RD�@RDȀRD��RD� RD�@RDɀRD��RD� RD�@RDʀRD��RD� RD�@RDˀRD��RD� RD�@RD̀RD��RD� RD�@RD̀RD��RD� RD�@RD΀RD��RD� RD�@RDπRD��RD� RD�@RDЀRD��RD� RD�@RDрRD��RD� RD�@RDҀRD��RD� RD�@RDӀRD��RD� RD�@RDԀRD��RD� RD�@RDՀRD��RD� RD�@RDրRD��RD� RD�@RD׀RD��RD��D�=D؀RD��RD� RD�@RDـRD��RD� RD�@RDڀRD��RD� RD�@RDۀRD��RD� RD�@RD܀RD��RD� RD�@RD݀RD��RD� RD�@RDހRD��RD� RD�@RD߀RD��RD� RD�@RD��RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD��RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD�ÅD��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�n�A�n�A�jA�l�A�n�A�n�A�n�A�p�A�n�A�n�A�p�A�n�A�p�A�p�A�p�A�p�A�r�A�r�A�r�A�r�A�r�A�t�A�t�A�v�A�x�A�x�A�jA��`A�bNA�%A�$�A�jA���A���A���A���A�{A�1'A��A���A��A�l�A�A��A�%A��A�A�A���A�&�A�p�A���A�VA�hsA���A�G�A�VA��A�bA���A��yA�/A���A��A�Q�A��`A�|�A��A�VA�bNA�&�A��mA��wA���A�9XA���A���A�C�A�A�ĜA�-A�=qA�I�A���A�&�A�5?A�^5A���A�1'A���A�9XA��uA�9XA��A��A~�uA}�mA}hsA|��A{"�Az��Ax�jAxbAw��Aw��AwS�Aw7LAw�AvĜAvAu��Au\)At^5Ap�An(�Am�Al��Al�\AlM�Ak�TAk�hAj�yAi�AiG�AhE�Ae��Ad��Ac�
Ac?}Abr�AaVA`1'A_K�A^�A]�wA]`BA]
=A\��A\z�A[`BAZM�AY/AX~�AX1AWt�AW�AV��AVA�AU�ASp�AR��ARE�AQ��AQ%AP��APĜAP�\AO"�AN{AMhsAL�yALjAK��AJ�AIt�AH��AG��AFbNAE�AD�AC��ACXABȴAB��ABz�ABM�AA�#AA`BA@bNA?K�A>��A>n�A>-A=��A=�A<z�A;��A:VA9��A8��A8I�A8(�A7�TA6�A5p�A2��A1��A1K�A0ĜA.�/A-�A,�jA,(�A,JA+�mA*�/A)�FA)`BA(��A(^5A'��A%O�A$A#�A#C�A"��A"��A"I�A!�A v�A�A�hA�`A-AA��A�AG�A�9AJA/A=qA�`A�AE�AI�A9XA�-A�A��A{A�AXAȴA �A��AJA
�A	�A$�A;dA~�A�;A��AJA��A�A Ĝ@��w@�p�@�9X@��@�{@��/@�-@�Q�@�`B@��@�Q�@�9X@��@�@��@��`@��;@�|�@��@��@�1'@�dZ@��H@�n�@�=q@�p�@�@�hs@��m@�;d@��@ް!@�~�@�^5@��T@ݩ�@݁@�?}@�V@�bN@���@���@ۍP@���@�^5@�x�@���@�E�@�r�@�C�@�ȴ@���@��@�1'@�33@�V@��#@�`B@���@�(�@˝�@�
=@ʗ�@�-@�hs@ȋD@ǝ�@�"�@�=q@�j@�C�@��H@�~�@���@�&�@��@�;d@�@�p�@�V@�A�@�|�@���@���@�/@�ƨ@��!@�@��/@�Q�@�1@�|�@��@��@�bN@�33@�-@���@�x�@���@�I�@���@���@�5?@�x�@�%@��`@�Ĝ@���@�=q@���@���@��
@��P@�"�@��\@�@���@���@�J@�/@�1@�t�@��@�5?@���@��@�Z@��
@�l�@���@��\@�=q@�@�O�@�bN@���@�5?@��#@���@���@�hs@�G�@�7L@�/@��`@��u@�I�@��@���@��y@�@�p�@���@��@��`@��9@��@��`@�r�@�K�@���@��H@�ȴ@�M�@�{@��T@�J@���@��^@��@���@�9X@��
@��w@��F@�ȴ@�n�@���@�@��-@���@�p�@�`B@�G�@�?}@��@��j@��@�j@� �@���@��y@���@�V@���@��7@�V@�%@��9@�I�@��@�P@�@��@~�y@}�@|�@|��@|��@|�D@{�
@{@z�\@y�@yX@y&�@x�9@x1'@w�P@wK�@w+@v��@vv�@v@uO�@t�/@t�/@t��@t��@t�D@tZ@t9X@sƨ@r^5@q�^@q%@p  @o��@ol�@ol�@ol�@o\)@o\)@ol�@o\)@o+@nV@m@m�@l�j@lZ@k�@j��@i�^@ix�@iG�@i&�@h��@hr�@hQ�@h �@gK�@f5?@e��@e��@eO�@d�j@d9X@c�m@c��@cC�@co@b�H@b�@c@b�@bn�@a��@ahs@aG�@ahs@a�7@a%@`��@`r�@` �@`  @_��@^�y@^�R@^v�@^$�@]��@]O�@]?}@\��@\z�@\9X@[�m@[dZ@[33@[ƨ@\�@\(�@\(�@\�@[��@["�@Z�@Z~�@ZJ@Y��@YX@X��@W�@W|�@V�@V�R@U�T@U/@T�@T(�@T(�@T(�@T9X@T�@S��@S�m@Sƨ@SdZ@S�@S"�@R�@R�!@R�\@R��@RM�@Q��@Q�#@Q�#@Q�#@Q��@Qhs@Q&�@P�9@P  @O��@O\)@O;d@N�y@NE�@N5?@N$�@N$�@M�h@L��@L�D@Lj@LI�@L9X@KdZ@J�H@J~�@J^5@I7L@I%@H�`@H��@I�^@J=q@I��@H�@H �@G�w@F�y@Fv�@FV@F��@FE�@F@E��@E?}@D�/@D�D@D(�@C��@Ct�@CdZ@CS�@CC�@C33@C"�@B�@B�\@B�@A�^@Ax�@AX@A7L@@��@@ �@?\)@?
=@>�y@>ȴ@>��@>V@=��@=p�@=�@<�j@<��@<j@<I�@<(�@<1@;�
@;��@;C�@;"�@;@:�H@:��@:�!@:~�@:M�@:-@:J@9��@9��@9�#@9��@9�#@9��@9x�@9hs@9X@9G�@9�@8��@8�@7�;@7l�@7\)@7+@7�@7
=@6�y@6V@5�@5��@5�@4�D@3�F@3dZ@333@333@3o@2�H@2��@2�!@2�!@2�!@2��@2~�@2=q@1�^@1hs@1X@1&�@0��@0��@0Ĝ@0Ĝ@0��@0�@0Q�@0 �@/�;@/�@/�P@/l�@/;d@/
=@.ff@-�T@-�h@-�@,Z@+ƨ@+C�@*�H@*�!@*~�@*=q@*�@*J@)��@)�7@)x�@)G�@(Ĝ@(�u@(Q�@'�P@'��@';d@&��@&�+@&V@%�-@%?}@%V@$�@$�/@$�/@$�/@$�@$��@$�/@$�@$(�@#��@"��@"^5@"=q@"=q@"~�@"�@!hs@!hs@!X@!X@!X@!X@!7L@!7L@ ��@ r�@  �@�;@�P@��@ȴ@�+@v�@ff@E�@{@�@��@�-@�@`B@?}@V@�@I�@��@��@�@dZ@S�@@n�@�#@%@�u@r�@�@�@1'@�@�w@�@�P@�P@l�@\)@�@
=@ȴ@��@�+@v�@@�-@�@O�@/@��@�j@j@1@dZ@"�@o@o@o@o@��@�!@�\@~�@^5@��@�^@x�@7L@bN@�@�@�@�P@\)@�@�R@v�@$�@{@{@@@@��@�@p�@p�@O�@/@�@��@��@Z@I�@I�@(�@1@�m@ƨ@�@C�@
�H@
�\@
�\@
~�@
n�@
^5@
M�@
=q@
�@	�^@	��@	�7@	x�@	G�@	�@	%@�`@��@Ĝ@�u@�u@�@�@r�@bN@  @�@�@�;@�;@�;@�;@�;@�;@�;@��@�P@;d@��@ȴ@�R@�R@v�@$�@@��@�-@��@�@O�@?}@/@/@�@�@��@�@Z@Z@I�@I�@�m@t�@C�@33@o@�@��@��@��@~�@^5@=q@-@-@�@J@��@�#@��@�^@��@G�@ ��@ Ĝ@ �@  �@   ?�;d?��?��R?��R?�v�?��?�/?��D?�j?�(�?�(�111111111111111111111111114411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�n�A�n�A�jA�l�A�n�A�n�A�n�A�p�A�n�A�n�A�p�A�n�A�p�A�p�A�p�A�p�A�r�A�r�A�r�A�r�A�r�A�t�A�t�A�v�A�x�A�x�G�O�G�O�A�bNA�%A�$�A�jA���A���A���A���A�{A�1'A��A���A��A�l�A�A��A�%A��A�A�A���A�&�A�p�A���A�VA�hsA���A�G�A�VA��A�bA���A��yA�/A���A��A�Q�A��`A�|�A��A�VA�bNA�&�A��mA��wA���A�9XA���A���A�C�A�A�ĜA�-A�=qA�I�A���A�&�A�5?A�^5A���A�1'A���A�9XA��uA�9XA��A��A~�uA}�mA}hsA|��A{"�Az��Ax�jAxbAw��Aw��AwS�Aw7LAw�AvĜAvAu��Au\)At^5Ap�An(�Am�Al��Al�\AlM�Ak�TAk�hAj�yAi�AiG�AhE�Ae��Ad��Ac�
Ac?}Abr�AaVA`1'A_K�A^�A]�wA]`BA]
=A\��A\z�A[`BAZM�AY/AX~�AX1AWt�AW�AV��AVA�AU�ASp�AR��ARE�AQ��AQ%AP��APĜAP�\AO"�AN{AMhsAL�yALjAK��AJ�AIt�AH��AG��AFbNAE�AD�AC��ACXABȴAB��ABz�ABM�AA�#AA`BA@bNA?K�A>��A>n�A>-A=��A=�A<z�A;��A:VA9��A8��A8I�A8(�A7�TA6�A5p�A2��A1��A1K�A0ĜA.�/A-�A,�jA,(�A,JA+�mA*�/A)�FA)`BA(��A(^5A'��A%O�A$A#�A#C�A"��A"��A"I�A!�A v�A�A�hA�`A-AA��A�AG�A�9AJA/A=qA�`A�AE�AI�A9XA�-A�A��A{A�AXAȴA �A��AJA
�A	�A$�A;dA~�A�;A��AJA��A�A Ĝ@��w@�p�@�9X@��@�{@��/@�-@�Q�@�`B@��@�Q�@�9X@��@�@��@��`@��;@�|�@��@��@�1'@�dZ@��H@�n�@�=q@�p�@�@�hs@��m@�;d@��@ް!@�~�@�^5@��T@ݩ�@݁@�?}@�V@�bN@���@���@ۍP@���@�^5@�x�@���@�E�@�r�@�C�@�ȴ@���@��@�1'@�33@�V@��#@�`B@���@�(�@˝�@�
=@ʗ�@�-@�hs@ȋD@ǝ�@�"�@�=q@�j@�C�@��H@�~�@���@�&�@��@�;d@�@�p�@�V@�A�@�|�@���@���@�/@�ƨ@��!@�@��/@�Q�@�1@�|�@��@��@�bN@�33@�-@���@�x�@���@�I�@���@���@�5?@�x�@�%@��`@�Ĝ@���@�=q@���@���@��
@��P@�"�@��\@�@���@���@�J@�/@�1@�t�@��@�5?@���@��@�Z@��
@�l�@���@��\@�=q@�@�O�@�bN@���@�5?@��#@���@���@�hs@�G�@�7L@�/@��`@��u@�I�@��@���@��y@�@�p�@���@��@��`@��9@��@��`@�r�@�K�@���@��H@�ȴ@�M�@�{@��T@�J@���@��^@��@���@�9X@��
@��w@��F@�ȴ@�n�@���@�@��-@���@�p�@�`B@�G�@�?}@��@��j@��@�j@� �@���@��y@���@�V@���@��7@�V@�%@��9@�I�@��@�P@�@��@~�y@}�@|�@|��@|��@|�D@{�
@{@z�\@y�@yX@y&�@x�9@x1'@w�P@wK�@w+@v��@vv�@v@uO�@t�/@t�/@t��@t��@t�D@tZ@t9X@sƨ@r^5@q�^@q%@p  @o��@ol�@ol�@ol�@o\)@o\)@ol�@o\)@o+@nV@m@m�@l�j@lZ@k�@j��@i�^@ix�@iG�@i&�@h��@hr�@hQ�@h �@gK�@f5?@e��@e��@eO�@d�j@d9X@c�m@c��@cC�@co@b�H@b�@c@b�@bn�@a��@ahs@aG�@ahs@a�7@a%@`��@`r�@` �@`  @_��@^�y@^�R@^v�@^$�@]��@]O�@]?}@\��@\z�@\9X@[�m@[dZ@[33@[ƨ@\�@\(�@\(�@\�@[��@["�@Z�@Z~�@ZJ@Y��@YX@X��@W�@W|�@V�@V�R@U�T@U/@T�@T(�@T(�@T(�@T9X@T�@S��@S�m@Sƨ@SdZ@S�@S"�@R�@R�!@R�\@R��@RM�@Q��@Q�#@Q�#@Q�#@Q��@Qhs@Q&�@P�9@P  @O��@O\)@O;d@N�y@NE�@N5?@N$�@N$�@M�h@L��@L�D@Lj@LI�@L9X@KdZ@J�H@J~�@J^5@I7L@I%@H�`@H��@I�^@J=q@I��@H�@H �@G�w@F�y@Fv�@FV@F��@FE�@F@E��@E?}@D�/@D�D@D(�@C��@Ct�@CdZ@CS�@CC�@C33@C"�@B�@B�\@B�@A�^@Ax�@AX@A7L@@��@@ �@?\)@?
=@>�y@>ȴ@>��@>V@=��@=p�@=�@<�j@<��@<j@<I�@<(�@<1@;�
@;��@;C�@;"�@;@:�H@:��@:�!@:~�@:M�@:-@:J@9��@9��@9�#@9��@9�#@9��@9x�@9hs@9X@9G�@9�@8��@8�@7�;@7l�@7\)@7+@7�@7
=@6�y@6V@5�@5��@5�@4�D@3�F@3dZ@333@333@3o@2�H@2��@2�!@2�!@2�!@2��@2~�@2=q@1�^@1hs@1X@1&�@0��@0��@0Ĝ@0Ĝ@0��@0�@0Q�@0 �@/�;@/�@/�P@/l�@/;d@/
=@.ff@-�T@-�h@-�@,Z@+ƨ@+C�@*�H@*�!@*~�@*=q@*�@*J@)��@)�7@)x�@)G�@(Ĝ@(�u@(Q�@'�P@'��@';d@&��@&�+@&V@%�-@%?}@%V@$�@$�/@$�/@$�/@$�@$��@$�/@$�@$(�@#��@"��@"^5@"=q@"=q@"~�@"�@!hs@!hs@!X@!X@!X@!X@!7L@!7L@ ��@ r�@  �@�;@�P@��@ȴ@�+@v�@ff@E�@{@�@��@�-@�@`B@?}@V@�@I�@��@��@�@dZ@S�@@n�@�#@%@�u@r�@�@�@1'@�@�w@�@�P@�P@l�@\)@�@
=@ȴ@��@�+@v�@@�-@�@O�@/@��@�j@j@1@dZ@"�@o@o@o@o@��@�!@�\@~�@^5@��@�^@x�@7L@bN@�@�@�@�P@\)@�@�R@v�@$�@{@{@@@@��@�@p�@p�@O�@/@�@��@��@Z@I�@I�@(�@1@�m@ƨ@�@C�@
�H@
�\@
�\@
~�@
n�@
^5@
M�@
=q@
�@	�^@	��@	�7@	x�@	G�@	�@	%@�`@��@Ĝ@�u@�u@�@�@r�@bN@  @�@�@�;@�;@�;@�;@�;@�;@�;@��@�P@;d@��@ȴ@�R@�R@v�@$�@@��@�-@��@�@O�@?}@/@/@�@�@��@�@Z@Z@I�@I�@�m@t�@C�@33@o@�@��@��@��@~�@^5@=q@-@-@�@J@��@�#@��@�^@��@G�@ ��@ Ĝ@ �@  �@   ?�;d?��?��R?��R?�v�?��?�/?��D?�j?�(�?�(�111111111111111111111111114411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B��B49Bl�B�7BffB}�B�\Bo�B�=B�Bu�BhsBP�B�B0!BVB�
B�B��B�B�fB�#B��B��BǮBƨB�}B�?B�B�B��B�VB_;BT�BQ�B:^B/BG�B<jB6FB�BD�BC�BB�B@�B7LB'�BbB%�B"�B�B\B
��B
�B
�B
�TB
��B
ǮB
ŢB
�LB
�'B
�B
��B
��B
�{B
�DB
~�B
�B
|�B
w�B
gmB
jB
ZB
\)B
aHB
^5B
^5B
]/B
ZB
VB
L�B
J�B
C�B
1'B
hB
JB
�B
�B
�B
�B
�B
hB

=B
B	��B	�B	�5B	�HB	�;B	�/B	��B	��B	��B	ǮB	ĜB	B	ÖB	��B	�wB	�^B	�B	��B	��B	��B	��B	��B	��B	��B	�uB	�7B	}�B	� B	�B	~�B	|�B	~�B	}�B	x�B	iyB	ffB	gmB	dZB	aHB	]/B	O�B	F�B	D�B	>wB	7LB	7LB	2-B	.B	,B	,B	-B	-B	)�B	$�B	�B	�B	{B	�B	�B	�B	oB	VB		7B	B��B��B��B��B��B��B�B�#B��B��B�B��BB�^BǮBĜBƨBÖB�XB�9B�^B�9B�B��B�{B��B��B��B��B��B��B�=B�JB�JB�7B�Bl�B�B�%B�B� Bz�Bu�Bp�Bn�BffBp�Br�Bs�Bo�BgmBZBVB[#BffBe`B_;BW
BB�B;dBD�B;dB;dB:^B?}B;dB9XB33B5?B2-B49B49B0!B49B33B5?B/B%�B(�B#�B0!B49B5?B33B1'B)�B)�B/B/B-B&�B-B/B1'B0!B0!B)�B�B"�B$�B+B/B0!B0!B0!B/B0!B0!B/B/B,B-B0!B-B)�B'�B#�B�B�B�B&�B,B(�B)�B)�B+B.B1'B2-B1'B/B0!B49B49B49B33B1'B2-B5?B1'B-B5?B>wB=qB:^B:^B8RB;dB;dBB�BB�BC�BA�BE�BD�BC�BD�BI�BN�BO�BT�BW
BR�BO�BP�BYBXB[#BcTBdZBaHBe`Be`Be`Bl�Bm�Bp�Bt�Bq�BiyBn�Br�Bq�Bo�Bw�Bw�Bw�Bz�Bw�B{�Bx�B|�By�B|�B~�B� B� B�B�B�B�B�B�=B�JB�VB�{B��B��B��B��B��B��B��B��B�B�B��B�B�B�B�B�B�B�^B�jBÖBĜBĜBƨB��BɺBɺB��B�B�B�B�B�)B�NB�HB�TB�`B�B�B�B��B��B�B��B��B	B	B	B	B	B	B	B	B	B	B	+B	+B	1B	DB	uB	{B	�B	�B	�B	�B	�B	�B	#�B	,B	2-B	1'B	0!B	0!B	8RB	9XB	;dB	<jB	<jB	>wB	?}B	?}B	?}B	A�B	A�B	B�B	E�B	I�B	J�B	J�B	J�B	K�B	M�B	Q�B	XB	YB	ZB	\)B	^5B	^5B	^5B	^5B	bNB	cTB	dZB	iyB	k�B	l�B	m�B	n�B	n�B	n�B	m�B	l�B	l�B	n�B	q�B	t�B	v�B	w�B	x�B	z�B	}�B	~�B	� B	� B	�B	�B	�B	� B	�B	�+B	�1B	�1B	�7B	�DB	�VB	�\B	�bB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�'B	�-B	�9B	�9B	�9B	�FB	�FB	�LB	�RB	�wB	��B	��B	B	B	B	ÖB	ÖB	B	ÖB	ÖB	ĜB	ĜB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�#B	�)B	�5B	�5B	�;B	�NB	�TB	�ZB	�`B	�`B	�mB	�fB	�fB	�mB	�mB	�mB	�fB	�fB	�fB	�`B	�fB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	�B	�B	��B	��B	��B	��B	��B	��B
  B
  B	��B
B
B
B
B
B
B
B
B
B
B
B
B
+B
+B
+B
+B
+B
+B
+B
+B
	7B

=B

=B
DB

=B
DB
PB
\B
bB
bB
bB
bB
bB
oB
oB
oB
{B
uB
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
"�B
#�B
#�B
#�B
#�B
"�B
!�B
!�B
!�B
!�B
 �B
 �B
 �B
!�B
!�B
!�B
!�B
!�B
!�B
#�B
#�B
#�B
#�B
#�B
#�B
$�B
%�B
%�B
&�B
&�B
&�B
'�B
'�B
'�B
'�B
'�B
(�B
(�B
)�B
)�B
)�B
)�B
(�B
)�B
+B
+B
+B
-B
.B
.B
/B
0!B
0!B
1'B
1'B
0!B
1'B
1'B
1'B
1'B
2-B
33B
33B
5?B
6FB
5?B
6FB
6FB
6FB
8RB
:^B
:^B
;dB
;dB
<jB
<jB
<jB
=qB
=qB
=qB
>wB
>wB
=qB
=qB
>wB
>wB
>wB
?}B
B�B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
F�B
G�B
G�B
H�B
H�B
H�B
H�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
L�B
K�B
K�B
L�B
M�B
N�B
O�B
P�B
Q�B
R�B
R�B
S�B
S�B
S�B
S�B
S�B
S�B
T�B
T�B
VB
VB
VB
T�B
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
W
B
W
B
XB
XB
XB
XB
XB
XB
XB
XB
XB
XB
XB
XB
XB
YB
XB
YB
ZB
ZB
[#B
ZB
ZB
ZB
ZB
ZB
[#B
\)B
\)B
\)B
[#B
\)B
]/B
]/B
]/B
]/B
]/B
^5B
^5B
^5B
^5B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
`BB
aHB
aHB
aHB
aHB
`BB
`BB
`BB
aHB
aHB
bNB
bNB
bNB
bNB
bNB
cTB
cTB
cTB
dZB
dZB
dZB
dZB
cTB
cTB
dZB
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
dZB
dZB
dZB
e`B
ffB
ffB
ffB
ffB
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
iyB
iyB
iyB
iyB
iyB
jB
jB
jB
jB
jB
k�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
o�B
n�B
n�B
o�B
o�B
p�B
p�B
q�B
q�B
q�B
r�B
r�B
r�B
q�B
r�B
s�B
s�B
t�B
t�B
t�111111111111111111111111114411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�*B��B��B�0G�O�G�O�BA Bs�B�Bk�B��B��Bt9B��B�GBx�BkkBU�BB1�B&B�B�WB�2B�B�B��BּB�VB�lB��B�AB�fB��B�/B�2B��BeBXyBUB?.B2BH�B>B88B#:BD�BD3BCBA B8�B*�BuB&�B#�B�B B
�(B
�B
�B
�FB
�.B
ɆB
��B
�	B
�GB
�cB
�NB
��B
��B
��B
��B
��B
}�B
x�B
i_B
kkB
\)B
\�B
a|B
^�B
^�B
]dB
ZkB
V�B
M�B
K^B
DgB
3B
�B
B
�B
�B
�B
B
B
B
)B
AB	�B	�9B	��B	�B	�vB	�B	�9B	�pB	��B	��B	ŢB	�{B	�B	��B	��B	�B	��B	�DB	�B	��B	�\B	�dB	�)B	�KB	�FB	��B	�B	��B	��B	�B	}�B	HB	~BB	yrB	kQB	g�B	h>B	eB	a�B	^B	QhB	HKB	E�B	@ B	8�B	8RB	3MB	/5B	,�B	,�B	-]B	-CB	*eB	%�B	�B	�B	�B	EB		B	B	@B	(B	
=B	YB��B��B��B�xB�VB�fB�B�~B��B�9BںB�B�B�PB�B�SB��B�3B��B��B��B�B�B�RB�YB�
B�jB�'B�5B�7B�SB��B�6B�B�	B��Bo�B�uB�YB�mB��B{�Bv�Bq�BpBh
BqBr�Bs�BpBhXB[�BW�B\CBgBe�B`BBX�BE�B=�BF%B=<B="B;�B@�B<�B:�B4�B6�B3�B5�B5�B1�B5B49B6B0UB'�B*eB%�B0�B4�B5tB3�B1�B+B*�B/�B/�B-�B(
B-�B/�B1�B0�B0�B*�BjB#�B%�B+�B/iB0UB0UB0UB/�B0UB0UB/iB/OB,�B-]B0UB-wB*eB(�B$�B�B�B�B'�B,�B)�B*�B*�B+�B.�B1�B2�B1�B/�B0�B4�B4�B4�B3�B1�B2�B5�B2B.cB6B>�B=�B:�B:�B9$B<B<6BB�BCBD3BB'BF?BESBDgBE�BJ�BO�BP�BUgBWsBS�BP�BQ�BY�BX�B[�Bc�Bd�Ba�Be�Be�Bf2Bl�BnBp�Bt�BrBj�Bo5Bs3Br-BpUBxBx8BxlB{Bx�B|�By�B}�Bz�B}qB}B��B��B��B��B��B��B��B�rB��B��B��B�SB��B�!B�B�B�B�$B�B�B�B�KB�QB�]B�cB��B��B��B��B��BðBĶB��B��B��B�=BʌB�<B�B�EB�mB�EB�]B�NB�|B�nB��B��B� B�B��B��B�MB�B�6B	 B	3B	3B	MB	3B	3B	3B	GB	[B	SB	_B	zB	�B	�B	�B	�B	�B	�B	�B	�B	B	!B	$&B	,B	2-B	1[B	0oB	0�B	8lB	9rB	;dB	<�B	<�B	>�B	?�B	?�B	?�B	A�B	A�B	B�B	E�B	I�B	J�B	J�B	J�B	K�B	N"B	RB	XB	YB	Z7B	\CB	^OB	^OB	^�B	^�B	b�B	c�B	d�B	i�B	k�B	lqB	mwB	n}B	n�B	n�B	m�B	l�B	l�B	n�B	q�B	t�B	v�B	xB	y>B	{B	~B	B	�B	�B	�B	� B	�;B	�iB	�uB	�EB	�1B	�fB	�lB	�xB	�pB	�vB	�}B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	��B	�*B	�*B	�/B	�B	�UB	�[B	�aB	�TB	�TB	�nB	�`B	�`B	�fB	�lB	�(B	�iB	��B	B	ªB	ªB	��B	ðB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�"B	�B	�@B	�
B	�B	�B	�=B	�CB	�OB	�5B	�VB	�4B	�nB	�tB	�`B	�zB	�RB	�B	�B	�RB	�mB	�mB	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�B	��B	��B	��B	��B	�PB	�VB
 4B
 4B	�HB
 B
 B
B
GB
-B
-B
AB
AB
B
GB
3B
mB
B
+B
+B
EB
EB
EB
_B
EB
	RB

XB

XB
^B

rB
xB
�B
vB
}B
}B
bB
}B
�B
oB
�B
�B
{B
�B
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
"�B
#�B
#�B
#�B
#�B
"�B
!�B
!�B
!�B
!�B
!B
!B
 �B
!�B
!�B
!�B
!�B
!�B
!�B
#�B
#�B
#�B
#�B
#�B
#�B
$�B
%�B
%�B
'B
&�B
'B
'�B
(
B
(
B
'�B
($B
)B
)B
)�B
*B
*B
*B
)DB
*0B
+6B
+6B
+6B
-CB
.IB
./B
/5B
0;B
0;B
1AB
1'B
0UB
1AB
1AB
1AB
1[B
2GB
3MB
3MB
5%B
6`B
5tB
6`B
6`B
6zB
8lB
:xB
:xB
;dB
;dB
<jB
<PB
<PB
=�B
=qB
=�B
>�B
>�B
=�B
=�B
>wB
>]B
>�B
?�B
B�B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
F�B
G�B
G�B
H�B
H�B
H�B
H�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
L�B
K�B
LB
MB
M�B
N�B
O�B
P�B
RB
SB
R�B
S�B
TB
S�B
TB
TB
S�B
UB
T�B
VB
VB
VB
U2B
VB
W$B
W$B
W$B
W$B
W$B
W$B
W?B
W?B
X+B
XB
XB
XB
XB
X+B
XB
X+B
X+B
XB
X+B
X+B
XB
Y1B
X_B
Y1B
Z7B
Z7B
[=B
Z7B
Z7B
Z7B
Z7B
Z7B
[#B
\)B
\)B
\B
[#B
\CB
]IB
]B
]/B
]IB
]IB
^OB
^OB
^OB
^OB
_;B
_;B
_VB
_VB
_VB
_VB
_VB
_VB
_;B
_VB
`BB
a-B
aHB
aHB
aHB
`\B
`\B
`\B
abB
abB
b4B
bhB
bhB
bNB
bhB
cTB
cTB
cnB
dZB
dZB
dZB
dZB
cnB
cTB
d@B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
dtB
dZB
dtB
e`B
f�B
ffB
ffB
f�B
f�B
gmB
g�B
h�B
hsB
h�B
h�B
iyB
i_B
iyB
iyB
iyB
iyB
i�B
i�B
jeB
jeB
j�B
j�B
j�B
k�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
o�B
n�B
n�B
o�B
o�B
p�B
p�B
q�B
q�B
q�B
r�B
r�B
r�B
q�B
r�B
s�B
s�B
t�B
t�B
t�111111111111111111111111114411111111111111311111111111111111111111111111111111111111111111111111111111111111111311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111113111111111111111111111111111111111111111111111111111111111111111111111111111111111111111331111131111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�<Np;<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.01(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201710140033052017101400330520171014003305201806221231542018062212315420180622123154201804050427232018040504272320180405042723  JA  ARFMdecpA19c                                                                20171010093509  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20171010003512  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20171010003514  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20171010003514  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20171010003515  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20171010003515  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20171010003515  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcp2.8e                                                                20171010003515  QCF$                G�O�G�O�G�O�            4000JA  ARGQaqcp2.8e                                                                20171010003515  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20171010003515  QCF$                G�O�G�O�G�O�            4000JA  ARGQrqcpt16c                                                                20171010003516  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20171010003516                      G�O�G�O�G�O�                JA  ARUP                                                                        20171010005629                      G�O�G�O�G�O�                JM  ARGQrqcjv291                                                                20171010015032  QCP$                G�O�G�O�G�O�2DEB7C          JM  ARGQrqcjv291                                                                20171010015032  QCF$                G�O�G�O�G�O�004000          JM  ARGQJMQC2.0                                                                 20171010015011  CV  JULD            G�O�G�O�F�c�                JM  ARSQJMQC2.0                                                                 20171011000000  CF  PSAL_ADJUSTED_QCB�  D�  G�O�                JM  ARCAJMQC2.0                                                                 20171013153305  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20171013153305  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404192723  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622033154  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115121517                      G�O�G�O�G�O�                