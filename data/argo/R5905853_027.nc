CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:27:27Z creation;2022-06-04T17:27:27Z conversion to V3.1      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8<   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8@   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8H   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8L   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8T   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8\   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8d   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8h   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8p   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8t   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8x   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8|   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9|   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I@   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M0   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �P   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �@   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �`   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �<   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �@   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �D   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �H   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �L   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604172727  20220610131506  5905853                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA                                  2B  A   APEX                            8421                            2.11.2                          846 @������1   @��q�@.��hr��c~��"�1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @�  A   A   A@  A`  A�  A�  A�  A���A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B?��BJffBP  BW��Bb��Bg��Bo��Bx  B�  B�  B���B���B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�ffB�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C�C�fC  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&�C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb�Cd�Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^fD^� D_  D_� D`  D`� Da  Da� DbfDb� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@=q@�Q�@�Q�A (�A (�A@(�A`(�A�{A�{A�{A��HA�{A�{A�{A�{B 
=B
=B
=B
=B 
=B(
=B0
=B8
=B?��BJp�BP
=BW��Bb�
Bg��Bo��Bx
=B�B�B���B���B���B���B�B�B�B�B�B�B�B�B�k�B�B�B�B���B�B�B�B�B�B�B�B�B�B�B�B�B�C �C�C)C��C�C
�C�C�C�C�C�C�C�C�C�C�C �C"�C$�C&)C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb)Cd)Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�C�C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�C�C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC��{C�HC�HC�HC�HD  �D ��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D	 �D	��D
 �D
��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D  �D ��D! �D!��D" �D"��D# �D#��D$ �D$��D% �D%��D& �D&��D' �D'��D( �D(��D) �D)��D* �D*��D+ �D+��D, �D,��D- �D-��D. �D.��D/ �D/��D0 �D0��D1 �D1��D2 �D2��D3 �D3��D4 �D4��D5 �D5��D6 �D6��D7 �D7��D8 �D8��D9 �D9��D: �D:��D; �D;��D< �D<��D= �D=��D> �D>��D? �D?��D@ �D@��DA �DA��DB �DB��DC �DC��DD �DD��DE �DE��DF �DF��DG �DG��DH �DH��DI �DI��DJ �DJ��DK �DK��DL �DL��DM �DM��DN �DN��DO �DO��DP �DP��DQ �DQ��DR �DR��DS �DS��DT �DT��DU �DU��DV �DV��DW �DW��DX �DX��DY �DY��DZ �DZ��D[ �D[��D\ �D\��D] �D]��D^
D^��D_ �D_��D` �D`��Da �Da��Db
Db��Dc �Dc��Dd �Dd��De �De��Df �Df��Dg �Dg��Dh �Dh��Di �Di��Dj �Dj��Dk �Dk��Dl �Dl��Dm �Dm��Dn �Dn��Do �Do��Dp �Dp��Dq �Dq��Dr �Dr��Ds �Ds��Dt �Dt��Du �Du��Dv �Dv��Dw �Dw��Dx �Dx��Dy �Dy��Dz �Dz��D{ �D{��D| �D|��D} �D}��D~ �D~��D �D��D� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RDRD��RD� RD�@RDÀRD��RD� RD�@RDĀRD��RD� RD�@RDŀRD��RD� RD�@RDƀRD��RD� RD�@RDǀRD��RD� RD�@RDȀRD��RD� RD�@RDɀRD��RD� RD�@RDʀRD��RD� RD�@RDˀRD��RD� RD�@RD̀RD��RD� RD�@RD̀RD��RD� RD�@RD΀RD��RD� RD�@RDπRD��RD� RD�@RDЀRD��RD� RD�@RDрRD��RD� RD�@RDҀRD��RD� RD�@RDӀRD��RD� RD�@RDԀRD��RD� RD�@RDՀRD��RD� RD�@RDրRD��RD� RD�@RD׀RD��RD� RD�@RD؀RD��RD� RD�@RDـRD��RD� RD�@RDڀRD��RD� RD�@RDۀRD��RD� RD�@RD܀RD��RD� RD�@RD݀RD��RD� RD�@RDހRD��RD� RD�@RD߀RD��RD� RD�@RD��RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD��RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111AߣAߜCAߡbAߣnAߤAߧ�AߥAߊ�A�~�A߁oA߅�A߈fA߇�A߉�A߉�Aߋ�Aߍ�Aߍ�Aߍ�Aߊ=A߇�A�cA�sMA�8�A�rA�7Aּ�A��A̫A�FA��TA��JA�$@A���A��A��$A���A�J#A��]A���A�h
A�:A���A��gA��}A�1A��A�-A��A�T�A��tA��A���A���A�ΥA���A�x8A�ʌA��TA���A�a�A�ŢA��~A��A�cTA���A��A���A�ݘA��*A�FA��Ay�As�#Ak\)Aj�UAi
�Ac��Ab�{AaVmA_�
A]Z�AZDgAX^�AV��ASd�AR�AR)_AQA�AN��AMe�AJ�sAHzxAD�QAC�$AC@AA�hA?�4A=SA;ƨA:��A9_�A7�A6��A6�A5�)A5^�A4@OA4+�A4A3�[A3h�A2k�A0�A-�)A,$A*��A)u%A'��A&�	A&4nA&�A%�WA&xA%��A%Q�A#'�A"�A#��A"�'A �A��A *0A �A �uA A��A��Ak�ASA��A�ZA�A�>A�A�4A�A�HAw2A6A��AH�A��A��A~(A��A�RA|A��A�Au�A��AA�A��A?}A�HA��A.�A�A�2A{�A�PA/A�A9XA	A��A�LA�AhsA�A��A
��A
��A
�A	�0A	C�A��Ag8AOA�A�AϫAY�A*�A�7A6zA��Ah�A�A��A9XA��A�AW�Ac�AGEA��Ah�A_A �hA �@���@�S�@��@��@��/@��z@��@�h�@�F�@�!�@��)@��D@���@�t�@�ی@�v�@�@�{@�/�@�ѷ@�3�@�[@�N<@��@��@�T�@��@�ȴ@�C-@븻@�M@�c�@�O�@���@�@��@��W@�h@���@�L0@�#:@�G@�@�l"@��@妵@��@�n@��X@�c�@�S�@�n@�@��[@ࠐ@��@�N�@�3�@��@�y�@���@�&@��E@ܓu@���@�|@��@��2@��@��@�]�@��`@�y>@׭C@���@�5?@��@ղ�@�C�@ԛ�@��+@Ӌ�@�2a@���@�Z�@��.@ю�@�t�@ц�@�-w@��K@�r�@�)�@��@�@��@�Q�@�͟@�q�@�@�ƨ@�W?@��@̄�@�#:@���@˸�@ˁ@��@�l�@�1�@���@�f�@��[@�W�@��d@�J#@ƃ@�	@�O@��a@���@ġb@ć+@��@�n/@��@�v�@�U2@�O@��@�a@���@���@�u%@��@��
@���@�P�@��@�	�@���@� \@�v�@��#@�G�@�o@��f@���@�z@��@�N<@��v@��6@��@� �@���@�c@�\�@�%@���@�tT@�*�@��o@��q@�t�@��@���@��@��>@��:@�n/@��E@� �@��"@�O�@��@�^5@���@�p�@�j�@�k�@�ں@�!�@��@�˒@��q@���@�s�@�J#@�6z@��@���@�_�@�A�@��/@���@�xl@�`�@�M�@�2�@��@�C�@��@��!@�Ov@��@���@�Y�@��U@���@�YK@�(�@�
�@���@���@�c�@�&@��@���@��0@�c@�c�@�=@��@���@��@��>@���@��@��$@�?�@���@��@�ں@�Ta@�J@�@�)_@���@��5@��I@�W�@��N@���@���@�c�@�/@��@�|�@�  @��@�X�@��h@�1'@��>@��6@��F@�X�@��@���@�\�@���@�hs@���@���@�r�@�
�@���@��	@�@O@���@�}V@��@��@���@���@��@@��V@�zx@�#�@��,@�q@�	@��6@�\�@�4�@���@���@�N�@�7@��z@�Vm@�%F@��/@��,@���@���@�Ĝ@���@���@���@�Q@���@�~�@�Dg@�*0@��P@���@���@�=q@���@�n/@�Q�@�B�@��@�xl@�]d@�'R@��>@���@��Q@��0@���@���@���@�H�@�ں@���@�{�@�C�@��+@���@�f�@�7L@��@�@��v@��r@�YK@�p;@��@��}@���@�=�@��c@��[@��h@���@��@�(�@��Q@�|@�B�@��@��_@�;�@��@+@~��@~�+@~L0@~O@}�d@}Vm@|�v@|�@|y>@{�+@{��@z�]@z=q@y��@y}�@yJ�@y�@x�@x�j@xA�@wRT@w
=@v�}@vl�@u��@u�@t�K@t�@t��@t>B@s�@sv`@r��@r��@r�@q?}@p�/@p��@p�@pj@ox@o@n��@n$�@mw2@l��@l�@k��@kO@kS@j�r@j�@iL�@h�)@h�e@h�@hN�@gخ@g\)@g;d@g�@f�@f��@fz@fff@f3�@fe@e�@e�h@eS&@e \@d�@dK^@c��@b��@bff@b1�@bJ@a�@a�@`�5@`tT@`Xy@`:�@`�@_�@_�@^�<@^d�@^O@]��@]X@]4@]/@]�@\��@\�@\��@\��@\y>@\>B@[�[@[�4@[9�@[@Z�@Z�R@Z8�@Z�@Y��@YL�@Y�@Y	l@X��@X6@W��@W�@V��@V�@U�@U�X@Uhs@U�@Toi@T�@S>�@R��@ROv@R0U@Q��@QS&@Q@P�f@P��@O�W@O�{@O33@O�@N��@NZ�@N
�@M�T@M�n@MY�@L��@L��@L��@LV�@L:�@K�$@J��@J��@Jxl@J�@J{@I��@I��@IT�@H��@H��@H[�@H4n@G˒@G9�@F�m@F� @Fl�@E��@E��@Ec�@EJ�@D�@Dj@D�@C�m@C� @C��@CS�@C/�@B�b@B:*@A@AL�@@�@@ �@?RT@? i@>��@>�+@>?@=��@=-w@<�f@<�@<�E@<��@<@;�A@;�W@;�&@;��@;qv@;�@:��@:Z�@::*@:.�@:
�@9��@9�-@9rG@9B�@8��@86@8�@7O@7�@6�@6��@6��@6��@6+k@5ϫ@5k�@5*0@5�@4��@4�E@4�@3��@3F�@3�@2��@2n�@2+k@2_@1��@1�o@1ԕ@1��@1Dg@1!�@0Ĝ@0��@0m�@0?�@09X@01'@/��@/��@/j�@.��@.�s@.�m@.{�@.Z�@.:*@.4@-�o@-��@-p�@-e,@-J�@,��@,��@,�@,N�@,�@+�{@+E9@+Y@*�@*�x@*YK@*1�@)�T@)ϫ@)�@)0�@(��@(�D@(Xy@(G@'��@'n/@'F�@'o@'
=@&��@&�,@&�F@&#:@%�o@%�C@%��@%x�@%O�@%#�@$��@#�+@#��@#�K@#�@#��@#U�@#!-@"��@"��@"d�@"{@!�)@!�j@!��@!\�@ �P@ �4@ c�@ ?�@ (�@��@��@��@iD@U�@8@��@L0@�@��@�@Ɇ@�u@I�@G@�0@|�@\)@�@�@�@�'@�}@��@c @6�@!�@�.@�@��@�>@��@�9@o @A @�|@��@�z@�@q@bN@A�@$@�@�]@�@�$@E9@6z@�@��@�1@�F@��@��@��@i�@?@�.@�@��@^�@Vm@N<@V@��@h�@-�@��@��@�@�@�!@��@q�@W�@3�@�@��@ԕ@�3@��@��@��@��@hs@A @%@Ĝ@��@z�@PH@4n@�W@��@��@n/@33@4�@�"@�@�@��@�+@;�@u@�j@��@��@��@rG@`B@N<@�@�[@��@�_111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111AߣAߜCAߡbAߣnAߤAߧ�AߥAߊ�A�~�A߁oA߅�A߈fA߇�A߉�A߉�Aߋ�Aߍ�Aߍ�Aߍ�Aߊ=A߇�A�cA�sMA�8�A�rA�7Aּ�A��A̫A�FA��TA��JA�$@A���A��A��$A���A�J#A��]A���A�h
A�:A���A��gA��}A�1A��A�-A��A�T�A��tA��A���A���A�ΥA���A�x8A�ʌA��TA���A�a�A�ŢA��~A��A�cTA���A��A���A�ݘA��*A�FA��Ay�As�#Ak\)Aj�UAi
�Ac��Ab�{AaVmA_�
A]Z�AZDgAX^�AV��ASd�AR�AR)_AQA�AN��AMe�AJ�sAHzxAD�QAC�$AC@AA�hA?�4A=SA;ƨA:��A9_�A7�A6��A6�A5�)A5^�A4@OA4+�A4A3�[A3h�A2k�A0�A-�)A,$A*��A)u%A'��A&�	A&4nA&�A%�WA&xA%��A%Q�A#'�A"�A#��A"�'A �A��A *0A �A �uA A��A��Ak�ASA��A�ZA�A�>A�A�4A�A�HAw2A6A��AH�A��A��A~(A��A�RA|A��A�Au�A��AA�A��A?}A�HA��A.�A�A�2A{�A�PA/A�A9XA	A��A�LA�AhsA�A��A
��A
��A
�A	�0A	C�A��Ag8AOA�A�AϫAY�A*�A�7A6zA��Ah�A�A��A9XA��A�AW�Ac�AGEA��Ah�A_A �hA �@���@�S�@��@��@��/@��z@��@�h�@�F�@�!�@��)@��D@���@�t�@�ی@�v�@�@�{@�/�@�ѷ@�3�@�[@�N<@��@��@�T�@��@�ȴ@�C-@븻@�M@�c�@�O�@���@�@��@��W@�h@���@�L0@�#:@�G@�@�l"@��@妵@��@�n@��X@�c�@�S�@�n@�@��[@ࠐ@��@�N�@�3�@��@�y�@���@�&@��E@ܓu@���@�|@��@��2@��@��@�]�@��`@�y>@׭C@���@�5?@��@ղ�@�C�@ԛ�@��+@Ӌ�@�2a@���@�Z�@��.@ю�@�t�@ц�@�-w@��K@�r�@�)�@��@�@��@�Q�@�͟@�q�@�@�ƨ@�W?@��@̄�@�#:@���@˸�@ˁ@��@�l�@�1�@���@�f�@��[@�W�@��d@�J#@ƃ@�	@�O@��a@���@ġb@ć+@��@�n/@��@�v�@�U2@�O@��@�a@���@���@�u%@��@��
@���@�P�@��@�	�@���@� \@�v�@��#@�G�@�o@��f@���@�z@��@�N<@��v@��6@��@� �@���@�c@�\�@�%@���@�tT@�*�@��o@��q@�t�@��@���@��@��>@��:@�n/@��E@� �@��"@�O�@��@�^5@���@�p�@�j�@�k�@�ں@�!�@��@�˒@��q@���@�s�@�J#@�6z@��@���@�_�@�A�@��/@���@�xl@�`�@�M�@�2�@��@�C�@��@��!@�Ov@��@���@�Y�@��U@���@�YK@�(�@�
�@���@���@�c�@�&@��@���@��0@�c@�c�@�=@��@���@��@��>@���@��@��$@�?�@���@��@�ں@�Ta@�J@�@�)_@���@��5@��I@�W�@��N@���@���@�c�@�/@��@�|�@�  @��@�X�@��h@�1'@��>@��6@��F@�X�@��@���@�\�@���@�hs@���@���@�r�@�
�@���@��	@�@O@���@�}V@��@��@���@���@��@@��V@�zx@�#�@��,@�q@�	@��6@�\�@�4�@���@���@�N�@�7@��z@�Vm@�%F@��/@��,@���@���@�Ĝ@���@���@���@�Q@���@�~�@�Dg@�*0@��P@���@���@�=q@���@�n/@�Q�@�B�@��@�xl@�]d@�'R@��>@���@��Q@��0@���@���@���@�H�@�ں@���@�{�@�C�@��+@���@�f�@�7L@��@�@��v@��r@�YK@�p;@��@��}@���@�=�@��c@��[@��h@���@��@�(�@��Q@�|@�B�@��@��_@�;�@��@+@~��@~�+@~L0@~O@}�d@}Vm@|�v@|�@|y>@{�+@{��@z�]@z=q@y��@y}�@yJ�@y�@x�@x�j@xA�@wRT@w
=@v�}@vl�@u��@u�@t�K@t�@t��@t>B@s�@sv`@r��@r��@r�@q?}@p�/@p��@p�@pj@ox@o@n��@n$�@mw2@l��@l�@k��@kO@kS@j�r@j�@iL�@h�)@h�e@h�@hN�@gخ@g\)@g;d@g�@f�@f��@fz@fff@f3�@fe@e�@e�h@eS&@e \@d�@dK^@c��@b��@bff@b1�@bJ@a�@a�@`�5@`tT@`Xy@`:�@`�@_�@_�@^�<@^d�@^O@]��@]X@]4@]/@]�@\��@\�@\��@\��@\y>@\>B@[�[@[�4@[9�@[@Z�@Z�R@Z8�@Z�@Y��@YL�@Y�@Y	l@X��@X6@W��@W�@V��@V�@U�@U�X@Uhs@U�@Toi@T�@S>�@R��@ROv@R0U@Q��@QS&@Q@P�f@P��@O�W@O�{@O33@O�@N��@NZ�@N
�@M�T@M�n@MY�@L��@L��@L��@LV�@L:�@K�$@J��@J��@Jxl@J�@J{@I��@I��@IT�@H��@H��@H[�@H4n@G˒@G9�@F�m@F� @Fl�@E��@E��@Ec�@EJ�@D�@Dj@D�@C�m@C� @C��@CS�@C/�@B�b@B:*@A@AL�@@�@@ �@?RT@? i@>��@>�+@>?@=��@=-w@<�f@<�@<�E@<��@<@;�A@;�W@;�&@;��@;qv@;�@:��@:Z�@::*@:.�@:
�@9��@9�-@9rG@9B�@8��@86@8�@7O@7�@6�@6��@6��@6��@6+k@5ϫ@5k�@5*0@5�@4��@4�E@4�@3��@3F�@3�@2��@2n�@2+k@2_@1��@1�o@1ԕ@1��@1Dg@1!�@0Ĝ@0��@0m�@0?�@09X@01'@/��@/��@/j�@.��@.�s@.�m@.{�@.Z�@.:*@.4@-�o@-��@-p�@-e,@-J�@,��@,��@,�@,N�@,�@+�{@+E9@+Y@*�@*�x@*YK@*1�@)�T@)ϫ@)�@)0�@(��@(�D@(Xy@(G@'��@'n/@'F�@'o@'
=@&��@&�,@&�F@&#:@%�o@%�C@%��@%x�@%O�@%#�@$��@#�+@#��@#�K@#�@#��@#U�@#!-@"��@"��@"d�@"{@!�)@!�j@!��@!\�@ �P@ �4@ c�@ ?�@ (�@��@��@��@iD@U�@8@��@L0@�@��@�@Ɇ@�u@I�@G@�0@|�@\)@�@�@�@�'@�}@��@c @6�@!�@�.@�@��@�>@��@�9@o @A @�|@��@�z@�@q@bN@A�@$@�@�]@�@�$@E9@6z@�@��@�1@�F@��@��@��@i�@?@�.@�@��@^�@Vm@N<@V@��@h�@-�@��@��@�@�@�!@��@q�@W�@3�@�@��@ԕ@�3@��@��@��@��@hs@A @%@Ĝ@��@z�@PH@4n@�W@��@��@n/@33@4�@�"@�@�@��@�+@;�@u@�j@��@��@��@rG@`B@N<@�@�[@��@�_111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111Bm�Bl"Bl�BlqBl=Bl�BlWBjeBiyBj�Bm�BoBo�BqBq�Bs�Bu?BwBw�Bx8By$By>Bz^BcB	&LB	MB	i�B	��B	��B
	B
�B
�B
8�B
��B
��B
ӏB
�DB#:B@iB8�B1�BSuBH1BK�Bh�Bq�B�VB�)B��BÖB��B��B�B�B�^B�B��B�B��B��B��BsMBW�BIlB$B
H�B	�mB	��B	�B	�dB	�FB	��B	�+B	q�B	J�B	BuB	@B	(
B	"�B	 'B	CB	�B		�B��B�B�B�kB�B�,B�dBּB͟B��B��B�$B��B��B��B��B��B��B�sB��B��B�ZB�?B�	B��B��B��B�OB�B��B�B��B�B�B�|B�BڠB�WB��B�B	�B	<�B	F%B	CGB	O\B	f�B	c�B	^�B	f�B	v�B	~�B	��B	�.B	�2B	� B	��B	��B	�1B	��B	��B	��B	�B	�XB	�yB	�DB	�0B	��B	��B	��B	�wB	�}B	�B	�!B	�B	��B	��B	��B	��B	��B	�^B	��B	��B	�aB	��B	�tB	żB	�KB	˒B	ʌB	��B	�B	�\B	՛B	��B	��B	ՁB	��B	�{B	�FB	�?B	�aB	��B	�:B	�B	ЗB	�@B	�9B	�2B	�B	�sB	��B	�B	��B	�7B	ٚB	�B	��B	�SB	��B	��B	� B	�VB	ΊB	�,B	��B	�?B	��B	��B	��B	��B	�
B	��B	��B	�)B	��B	چB	��B	ܒB	�IB	�B	ܬB	�]B	�!B	��B	��B	��B	��B	�vB	ߤB	�!B	�B	�\B	ߊB	�'B	��B	�B	��B	��B	�4B	�B	�NB	��B	�HB	�B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	�B	�B	��B	�B	��B	��B	�B	�B	�B	�%B	�?B	��B	��B	�B	�B	��B	�'B	�B	�B	�B	�B	�TB	�GB	��B	�B	�IB	�wB	�)B	�B	�cB	�B	�5B	�B	�B	�B	�B	��B	�B	�B	��B	��B	�B	��B	��B	��B	�B	�aB	�GB	�B	�GB	�-B	�B	�-B	��B	�B	�nB	�+B	��B	��B	��B	�`B	�LB	��B	��B	�B	��B	��B	�`B	�`B	��B	��B	�TB	�%B	��B	�B	�B	�B	�B	��B	�B	��B	�fB	��B	��B	��B	��B	��B	��B	�2B	��B	��B	��B	�B	��B	��B	�*B	�$B	��B	��B	��B	��B	�XB	��B	�XB	�B	�DB	��B	��B	��B	�0B	��B	�B	��B	�jB	�PB	�B	�PB	�jB	��B	�"B	�B	��B	��B	�qB	�"B	�<B	��B	�VB	�qB	��B	��B	�wB	��B	��B	�(B	��B
 �B
�B
�B
�B
AB
aB
aB
aB
�B
MB
B
�B
�B
YB
�B
�B
�B
�B
tB
_B
�B
	B
�B
�B
KB

�B

=B
0B
B
�B
0B
xB
�B
B
dB
~B
JB
�B
�B
VB
"B
�B
�B
�B
B
�B
�B
B
"B
�B
\B
BB
\B
vB
�B
bB
HB
.B
HB
.B
bB
 B
@B
�B
,B
FB
{B
{B
�B
aB
[B
�B
2B
B
B
�B

B
�B
�B
�B
_B
YB
�B
KB
B
eB
kB
�B
�B
�B
�B
�B
xB
�B
�B
�B
�B
5B
jB
�B
 'B
 'B
 �B
!�B
"�B
"�B
"�B
"�B
"�B
#nB
#nB
$@B
%�B
%�B
&2B
&LB
&�B
&�B
'�B
(XB
(�B
'�B
'�B
(>B
(>B
(
B
'�B
'8B
&�B
&�B
'8B
'�B
(sB
'mB
'8B
'RB
'�B
'�B
)B
)�B
*�B
+QB
+kB
+QB
,�B
,qB
,=B
+�B
+kB
+B
*�B
*�B
+6B
+�B
+�B
,WB
+�B
-�B
.�B
.�B
/ B
.�B
.�B
/ B
/5B
/�B
0�B
1vB
1�B
2-B
2B
1�B
2GB
2B
2|B
2|B
2GB
1�B
2GB
2aB
2aB
2|B
3B
3hB
3�B
3�B
3�B
4B
4�B
4�B
5B
5%B
5ZB
5ZB
5?B
5�B
6zB
6�B
6�B
6�B
7�B
8B
8B
8B
88B
8�B
8�B
8�B
9>B
9XB
9�B
:*B
:^B
:xB
:�B
:xB
;0B
;B
;�B
<B
<jB
="B
=VB
=�B
=�B
>B
>BB
>�B
?B
?.B
?cB
?}B
?�B
?�B
@iB
@OB
@iB
@iB
@�B
@�B
@�B
@�B
@�B
AB
AUB
AUB
AUB
AoB
AUB
A�B
AUB
B[B
B�B
B�B
CB
B�B
C�B
C�B
C�B
C�B
C�B
D3B
D�B
D�B
EB
EB
ESB
E�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
FtB
F�B
G+B
GB
GB
G+B
GB
G+B
GzB
G�B
H�B
H�B
H�B
H�B
HfB
H�B
IB
I�B
I�B
J	B
I�B
I�B
I�B
J#B
KB
KDB
KDB
K)B
J�B
J�B
J�B
J�B
J�B
KB
KxB
K�B
K�B
LB
L�B
M6B
MB
MjB
MPB
M�B
NB
NB
NB
NB
N<B
N"B
N�B
OB
OB
O\B
O�B
O�B
O�B
PB
PHB
PbB
P�B
P�B
P�B
Q4B
Q�B
R B
R B
RoB
S[B
S�B
TaB
TaB
T�B
T�B
T�B
T�B
T�B
U2B
U2B
U2B
U�B
U�B
VB
VSB
V�B
WYB
X+B
XyB
XyB
XyB
X_B
Y1B
Y�B
Y�B
Y�B
Y�B
Y�B
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
[=B
[=B
[�B
\B
\CB
\�B
\�B
\�B
\�B
\xB
\�B
\xB
\�B
]IB
]IB
]�B
]~B
]dB
]�B
^B
^OB
^�B
^�B
^�B
^�B
^�B
_�B
_�B
_�B
_�B
`vB
`\B
`�B
`�B
`�B
`�B
a-B
a�B
a�B
bB
bhB
bNB
b�B
b�B
b�B
b�B
b�B
b�B
b�B
b�B
b�B
b�B
c:B
cTB
c�B
c�B
c�B
c�B
c�B
c�B
c�B
dB
dB
d&B
dZB
d�B
e,B
eFB
ezB
ezB
e�B
e�B
e�B
fLB
fB
f�B
gB
g�B
g�B
h
B
hXB
h�B
h�B
iB
iDB
iDB
i_B
iDB
i�B
jB
i�B
jKB
jeB
jKB
jB
jB
j�B
k6B
k6B
kB
kB
kB
kkB
kkB
k�B
k�B
l"B
lqB
l�B
lqB
l�B
mB
m]B
m�B
m�B
m�B
nB
n/B
nIB
ncB
n}B
n}B
n}B
o B
o�B
o�B
pB
poB
p�B
p�B
q'B
qvB
q�B
q�B
rGB
r�B
r�B
r�B
r�B
sB
s3B
sMB
s�B
s�B
tB
tTB
tTB
t9B
tB
tTB
t�B
t�B
t�B
u%B
u?B
u%B
u%B
uZB
uZB
utB
utB
u�B
u�B
vB
v�B
v`B
v�B
v�B
wB
w2B
wB
wB
wB
wB
wLB
w2B
w�B
xB
xB
xB
w�B
xRB
x�B
x�B
x�B
y$B
yXB
yXB
yXB
y�B
y�B
zxB
z�B
z�B
{B
{0B
{JB
{dB
{B
{�B
{�B
{�B
|B
{�B
{�B
|6B
|B
|6B
|jB
|�B
|jB
|�B
|jB
|�B
|�B
}"B
}"B
}VB
}qB
}qB
}�B
~B
~(B
}�B
~B
~(B
~BB
~wB
~]B
~]B
B
~�B
B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111Bm�Bl"Bl�BlqBl=Bl�BlWBjeBiyBj�Bm�BoBo�BqBq�Bs�Bu?BwBw�Bx8By$By>Bz^BcB	&LB	MB	i�B	��B	��B
	B
�B
�B
8�B
��B
��B
ӏB
�DB#:B@iB8�B1�BSuBH1BK�Bh�Bq�B�VB�)B��BÖB��B��B�B�B�^B�B��B�B��B��B��BsMBW�BIlB$B
H�B	�mB	��B	�B	�dB	�FB	��B	�+B	q�B	J�B	BuB	@B	(
B	"�B	 'B	CB	�B		�B��B�B�B�kB�B�,B�dBּB͟B��B��B�$B��B��B��B��B��B��B�sB��B��B�ZB�?B�	B��B��B��B�OB�B��B�B��B�B�B�|B�BڠB�WB��B�B	�B	<�B	F%B	CGB	O\B	f�B	c�B	^�B	f�B	v�B	~�B	��B	�.B	�2B	� B	��B	��B	�1B	��B	��B	��B	�B	�XB	�yB	�DB	�0B	��B	��B	��B	�wB	�}B	�B	�!B	�B	��B	��B	��B	��B	��B	�^B	��B	��B	�aB	��B	�tB	żB	�KB	˒B	ʌB	��B	�B	�\B	՛B	��B	��B	ՁB	��B	�{B	�FB	�?B	�aB	��B	�:B	�B	ЗB	�@B	�9B	�2B	�B	�sB	��B	�B	��B	�7B	ٚB	�B	��B	�SB	��B	��B	� B	�VB	ΊB	�,B	��B	�?B	��B	��B	��B	��B	�
B	��B	��B	�)B	��B	چB	��B	ܒB	�IB	�B	ܬB	�]B	�!B	��B	��B	��B	��B	�vB	ߤB	�!B	�B	�\B	ߊB	�'B	��B	�B	��B	��B	�4B	�B	�NB	��B	�HB	�B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	�B	�B	��B	�B	��B	��B	�B	�B	�B	�%B	�?B	��B	��B	�B	�B	��B	�'B	�B	�B	�B	�B	�TB	�GB	��B	�B	�IB	�wB	�)B	�B	�cB	�B	�5B	�B	�B	�B	�B	��B	�B	�B	��B	��B	�B	��B	��B	��B	�B	�aB	�GB	�B	�GB	�-B	�B	�-B	��B	�B	�nB	�+B	��B	��B	��B	�`B	�LB	��B	��B	�B	��B	��B	�`B	�`B	��B	��B	�TB	�%B	��B	�B	�B	�B	�B	��B	�B	��B	�fB	��B	��B	��B	��B	��B	��B	�2B	��B	��B	��B	�B	��B	��B	�*B	�$B	��B	��B	��B	��B	�XB	��B	�XB	�B	�DB	��B	��B	��B	�0B	��B	�B	��B	�jB	�PB	�B	�PB	�jB	��B	�"B	�B	��B	��B	�qB	�"B	�<B	��B	�VB	�qB	��B	��B	�wB	��B	��B	�(B	��B
 �B
�B
�B
�B
AB
aB
aB
aB
�B
MB
B
�B
�B
YB
�B
�B
�B
�B
tB
_B
�B
	B
�B
�B
KB

�B

=B
0B
B
�B
0B
xB
�B
B
dB
~B
JB
�B
�B
VB
"B
�B
�B
�B
B
�B
�B
B
"B
�B
\B
BB
\B
vB
�B
bB
HB
.B
HB
.B
bB
 B
@B
�B
,B
FB
{B
{B
�B
aB
[B
�B
2B
B
B
�B

B
�B
�B
�B
_B
YB
�B
KB
B
eB
kB
�B
�B
�B
�B
�B
xB
�B
�B
�B
�B
5B
jB
�B
 'B
 'B
 �B
!�B
"�B
"�B
"�B
"�B
"�B
#nB
#nB
$@B
%�B
%�B
&2B
&LB
&�B
&�B
'�B
(XB
(�B
'�B
'�B
(>B
(>B
(
B
'�B
'8B
&�B
&�B
'8B
'�B
(sB
'mB
'8B
'RB
'�B
'�B
)B
)�B
*�B
+QB
+kB
+QB
,�B
,qB
,=B
+�B
+kB
+B
*�B
*�B
+6B
+�B
+�B
,WB
+�B
-�B
.�B
.�B
/ B
.�B
.�B
/ B
/5B
/�B
0�B
1vB
1�B
2-B
2B
1�B
2GB
2B
2|B
2|B
2GB
1�B
2GB
2aB
2aB
2|B
3B
3hB
3�B
3�B
3�B
4B
4�B
4�B
5B
5%B
5ZB
5ZB
5?B
5�B
6zB
6�B
6�B
6�B
7�B
8B
8B
8B
88B
8�B
8�B
8�B
9>B
9XB
9�B
:*B
:^B
:xB
:�B
:xB
;0B
;B
;�B
<B
<jB
="B
=VB
=�B
=�B
>B
>BB
>�B
?B
?.B
?cB
?}B
?�B
?�B
@iB
@OB
@iB
@iB
@�B
@�B
@�B
@�B
@�B
AB
AUB
AUB
AUB
AoB
AUB
A�B
AUB
B[B
B�B
B�B
CB
B�B
C�B
C�B
C�B
C�B
C�B
D3B
D�B
D�B
EB
EB
ESB
E�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
FtB
F�B
G+B
GB
GB
G+B
GB
G+B
GzB
G�B
H�B
H�B
H�B
H�B
HfB
H�B
IB
I�B
I�B
J	B
I�B
I�B
I�B
J#B
KB
KDB
KDB
K)B
J�B
J�B
J�B
J�B
J�B
KB
KxB
K�B
K�B
LB
L�B
M6B
MB
MjB
MPB
M�B
NB
NB
NB
NB
N<B
N"B
N�B
OB
OB
O\B
O�B
O�B
O�B
PB
PHB
PbB
P�B
P�B
P�B
Q4B
Q�B
R B
R B
RoB
S[B
S�B
TaB
TaB
T�B
T�B
T�B
T�B
T�B
U2B
U2B
U2B
U�B
U�B
VB
VSB
V�B
WYB
X+B
XyB
XyB
XyB
X_B
Y1B
Y�B
Y�B
Y�B
Y�B
Y�B
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
[=B
[=B
[�B
\B
\CB
\�B
\�B
\�B
\�B
\xB
\�B
\xB
\�B
]IB
]IB
]�B
]~B
]dB
]�B
^B
^OB
^�B
^�B
^�B
^�B
^�B
_�B
_�B
_�B
_�B
`vB
`\B
`�B
`�B
`�B
`�B
a-B
a�B
a�B
bB
bhB
bNB
b�B
b�B
b�B
b�B
b�B
b�B
b�B
b�B
b�B
b�B
c:B
cTB
c�B
c�B
c�B
c�B
c�B
c�B
c�B
dB
dB
d&B
dZB
d�B
e,B
eFB
ezB
ezB
e�B
e�B
e�B
fLB
fB
f�B
gB
g�B
g�B
h
B
hXB
h�B
h�B
iB
iDB
iDB
i_B
iDB
i�B
jB
i�B
jKB
jeB
jKB
jB
jB
j�B
k6B
k6B
kB
kB
kB
kkB
kkB
k�B
k�B
l"B
lqB
l�B
lqB
l�B
mB
m]B
m�B
m�B
m�B
nB
n/B
nIB
ncB
n}B
n}B
n}B
o B
o�B
o�B
pB
poB
p�B
p�B
q'B
qvB
q�B
q�B
rGB
r�B
r�B
r�B
r�B
sB
s3B
sMB
s�B
s�B
tB
tTB
tTB
t9B
tB
tTB
t�B
t�B
t�B
u%B
u?B
u%B
u%B
uZB
uZB
utB
utB
u�B
u�B
vB
v�B
v`B
v�B
v�B
wB
w2B
wB
wB
wB
wB
wLB
w2B
w�B
xB
xB
xB
w�B
xRB
x�B
x�B
x�B
y$B
yXB
yXB
yXB
y�B
y�B
zxB
z�B
z�B
{B
{0B
{JB
{dB
{B
{�B
{�B
{�B
|B
{�B
{�B
|6B
|B
|6B
|jB
|�B
|jB
|�B
|jB
|�B
|�B
}"B
}"B
}VB
}qB
}qB
}�B
~B
~(B
}�B
~B
~(B
~BB
~wB
~]B
~]B
B
~�B
B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104853  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604172727  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604172727  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604172727                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605022735  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605022735  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610131506                      G�O�G�O�G�O�                