CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:29:38Z creation;2022-06-04T17:29:38Z conversion to V3.1      
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
_FillValue                 �  I8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M(   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tx   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �0   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �p   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �(   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ޘ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �    HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �D   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �T   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �X   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �h   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �l   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �p   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20220604172938  20220610131507  5905853                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               'A   JA                                  2B  A   APEX                            8421                            2.11.2                          846 @�H�\(�1   @�I�m�5@/aG�z��b�$�/1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @�  A   A   A@  A^ffA~ffA�  A�  A�  A�  A���A�  A���B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�ffB���B�  B�  B�  B�  B�  B�  B�ffB�33B�  B�  B�ffB�  B�33B���B�  C   C  C  CL�C  C	��C�fC  C  C  C  C  C  C  C  C  C   C"�C$�C%�fC(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR�CT33CU�fCW�fCZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6y�D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D���D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�vf1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @=q@�Q�@�Q�A (�A (�A@(�A^�\A~�\A�{A�{A�{A�{A��HA�{A��HB 
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
=B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�8RB�k�B���B�B�B�B�B�B�B�k�B�8RB�B�B�k�B�B�8RB���B�C �C�C�CO\C�C	�\C��C�C�C�C�C�C�C�C�C�C �C")C$)C%��C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR)CT5�CU��CW��CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C�HC�HC�HC�HC�C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�C�C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC��{C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HD  �D ��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D	 �D	��D
 �D
��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D
D��D �D��D �D��D �D��D  �D ��D! �D!��D" �D"��D# �D#��D$ �D$��D% �D%��D& �D&��D' �D'��D( �D(��D) �D)��D* �D*��D+ �D+��D, �D,��D- �D-��D. �D.��D/ �D/��D0 �D0��D1 �D1��D2 �D2��D3 �D3��D4 �D4��D5 �D5��D6 �D6z>D7 �D7��D8 �D8��D9 �D9��D: �D:��D; �D;��D< �D<��D= �D=��D> �D>��D? �D?��D@ �D@��DA �DA��DB �DB��DC �DC��DD �DD��DE �DE��DF �DF��DG �DG��DH �DH��DI �DI��DJ �DJ��DK �DK��DL �DL��DM �DM��DN �DN��DO �DO��DP �DP��DQ �DQ��DR �DR��DS �DS��DT �DT��DU �DU��DV �DV��DW �DW��DX �DX��DY �DY��DZ �DZ��D[ �D[��D\ �D\��D] �D]��D^ �D^��D_ �D_��D` �D`��Da �Da��Db �Db��Dc �Dc��Dd �Dd��De �De��Df �Df��Dg �Dg��Dh �Dh��Di �Di��Dj �Dj��Dk �Dk��Dl �Dl��Dm �Dm��Dn �Dn��Do �Do��Dp �Dp��Dq �Dq��Dr �Dr��Ds �Ds��Dt �Dt��Du �Du��Dv �Dv��Dw �Dw��Dx �Dx��Dy �Dy��Dz �Dz��D{ �D{��D| �D|��D} �D}��D~ �D~��D �D��D� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�C�D��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD��D�@RDRD��RD� RD�@RDÀRD��RD� RD�@RDĀRD��RD��D�@RDŀRD��RD� RD�@RDƀRD��RD� RD�@RDǀRD��RD� RD�@RDȀRD��RD� RD�@RDɀRD��RD� RD�@RDʀRD��RD� RD�@RDˀRD��RD� RD�@RD̀RD��RD� RD�@RD̀RD��RD� RD�@RD΀RD��RD� RD�@RDπRD��RD� RD�@RDЀRD��RD� RD�@RDрRD��RD� RD�@RDҀRD��RD� RD�@RDӀRD��RD� RD�@RDԀRD��RD� RD�@RDՀRD��RD� RD�@RDրRD��RD� RD�@RD׀RD��RD� RD�@RD؀RD��RD� RD�@RDـRD��RD� RD�@RDڀRD��RD� RD�@RDۀRD��RD� RD�@RD܀RD��RD� RD�@RD݀RD��RD� RD�@RDހRD��RD� RD�@RD߀RD��RD� RD�@RD��RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD��RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD�v�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�I�A�IA�L0A�LdA�OBA�O�A�R�A�R�A�R�A�R�A�R A�T�A�U2A�VmA�X�A�^5A�^5A�\�A�^A�H�A�F�A�EmA�=<A�/�A�(XA�(�A�($A�)�A�<A�_A�a�A�m)A�sMA�w�A�|PA�|A�{JA�zxA�x�A�w�A�v`A�v`A�v+A�w2A�xlA�t�A�_�A��AƸA�d�A�7�A�(XA��A��QA�8�A½�A�j�A���A�D�A�zDA��eA��"A��jA��A�<A��A���A���A�4�A���A��A�`A�(A���A�wfA��*A�\)A� �Am�Ay�"Avm]Aot�AcZA\oiAZ�7AWQ�AT��ARv�AP�ANh
AL5�AH@�ACb�AAc A@�.A?JA<�AA9��A7��A7ƨA7U2A6�8A6r�A5��A3�A2$�A/��A.�A-��A-��A,�)A+�bA+*0A+�A)�A)7A(��A(YKA&��A&@A%A$,�A#]�A"��A#DgA#{A"A!�fA!�pA!^�A �MA ��A 1A�CAJ#A!�A��A��A�HAg8A��A��Az�ATaA��AbNA+A��A4�A��A�A�LA�AxlA�DAA_pA��Ay�AuA!�A�[A=qA�>A�DA�A��AN<A�AjA�mA~�AA�A�Ao A��AiDA'�A
�A
|�A	��A	Q�A	�A	Z�A	XyA�A�A=qAںAffA�-Ah�A�A�Aw�A�+A4A�MA�AA A��A+kA ��A ��A �A �}A O�A 8�@��
@��@�J@��c@���@�/�@��@���@��@�W�@�A�@��E@��@���@�m]@��@��P@�xl@�/�@�ی@��@�w�@�X@��@��
@�ی@�a�@�˒@��@��[@���@�@�R�@�[�@��A@��@�q@�YK@�u@�j@��m@鞄@��U@��#@��@�:�@彥@��@�F@�,�@�#�@�'�@�`�@��@��@��@��@��[@��@��@��@�n@�(@���@��@��@��@�c@�kQ@ݼ�@�j�@ܝI@�˒@�p�@�`B@��@�_�@ٱ[@�s@�҉@��@��@�_p@�F�@خ}@�Q�@�x�@ր�@�|@���@Ԓ�@�!-@�s@ի�@�|@�/�@�:�@�hs@�7L@�@�˒@Ԉ�@��@�9�@�c�@�(�@���@�5?@�(�@��@���@��@ϯ�@�2�@��@̗�@̔F@�_�@��@˙�@�T�@��@�7�@�4@ˠ'@��@�u�@�D�@��T@ɄM@�F@���@��?@ȣ�@�h
@��@�x�@�[W@��f@ƚ�@�~�@�V�@�4n@��K@�5�@��m@�z@�PH@�,=@�_@���@��6@ò�@Ë�@�V@��U@x@�/�@���@��@���@�u�@��5@��@���@���@�\�@��]@��@���@�s�@�6z@�,�@�&�@��@�@���@�~�@�>B@���@�X�@��@��b@�^5@�1@��6@��w@���@�N<@��@���@��@���@�m]@�:�@��@�Ov@���@�F@��@���@�q�@�%�@���@��X@��@��m@��@�u�@���@�^5@�@��'@��@�	@���@�iD@���@�GE@���@��k@�G�@�@��|@���@�u%@�4n@�{@���@�j@�Xy@�IR@���@�N�@�@���@�O@�;@��E@��e@�u%@�\�@�G@��j@��k@�_p@��@��p@���@�u�@�*�@��;@���@���@�"�@�B[@�}�@�.I@��f@���@��j@��_@���@�t�@�F@�%F@�S@��@��h@�r�@�@�@��@� �@�1�@�2�@�C�@���@�w2@�n/@�rG@�x@�P�@��@��x@�e�@� �@��)@��t@���@�u�@�@O@�!-@��6@�1�@���@�e,@��K@��r@�=q@��@��T@��
@�g�@���@��)@���@�4@���@��@��k@�%F@��,@���@�bN@�,=@��N@��@�a�@��@���@���@��@�g8@�M@��@���@�j@���@��1@�V�@��@��}@���@�Dg@��@��@��@�h�@�-�@��@���@��~@�x�@�w2@�e,@�A�@�1�@��@���@�ں@��_@�I�@��t@�!-@�ی@�U2@�e@��@��'@���@�RT@�V@���@�~�@�1'@��@��@���@�33@���@��@��\@�N�@�7@��@�خ@��F@�}�@�L�@��@��@���@��.@�h�@�!@��@���@��@��h@��~@��4@�|@�rG@�f�@�_p@�P�@�0�@��@��@�֡@��h@�W�@�*�@��@�m@˒@�w@�k@y�@�f@v`@qv@j�@E9@,�@~�"@~�1@~-@}��@}�-@}�~@}�@{��@{C�@{
=@z�@z�@z{�@zs�@z_�@y�Z@x�$@xXy@xK^@x2�@x�@w��@v��@vR�@v�@u��@uo @t��@t�O@t|�@tm�@t~@s�V@sH�@s!-@s�@r�M@r3�@q[W@p�D@o�@o�@o��@n�2@nJ�@mx�@l�)@l�@kb�@k$t@j��@j�@i�d@i�X@ic�@h��@h��@h1'@g�W@g�4@g�@f&�@ex�@e4@d��@d�@c�V@cZ�@c�@bl�@b{@a@a��@a��@azx@`֡@`q@`I�@_�q@_A�@^͟@]�@]x�@\��@\,=@[��@[J#@Z
�@Y�7@Y	l@X��@X�@W�$@Wn/@W8@V�<@V#:@U�@UQ�@T�j@TU2@T�@S��@Sb�@SK�@S@O@S&@R�@Rd�@R_@Q��@Q�@Q��@Q}�@Q=�@Q;@P�9@PQ�@O��@O��@OC@N�b@Nc @NTa@M��@M��@L�5@L��@Loi@L>B@L!@LM@L	�@Kݘ@K��@Ka@K/�@K�@J��@JTa@J:*@I�T@I�@H��@Hc�@G��@G�Q@GJ#@GC@G�@G�@F��@F�]@F��@F}V@Fn�@FZ�@FC�@F.�@F	@E�@E�'@D�@DZ@D�@C�@C�@C_p@B�c@B��@BZ�@A�S@A-w@A�@@��@@��@@�u@@z�@@2�@?�@?��@?~�@?C�@>��@>�L@>i�@=�@=5�@<�`@<��@<�O@<[�@;v`@;)_@:�H@:�<@:�}@:� @:n�@:3�@9��@94@8�@84n@7�F@7l�@7�@6�@6V@5��@5�@5��@5}�@5�@4�@4ی@4��@4�@4l"@4"h@3�@3��@3�@3W?@3!-@2�@2�!@1�o@0��@0�u@0*�@/�w@/��@/F�@.�"@.�@.�r@.6�@-��@-��@-<6@,��@,S�@,/�@+��@+qv@+O@+/�@+�@+@*�y@*�1@)�3@)��@)IR@(�5@(Ɇ@(�@(��@(z�@(?�@(G@'��@&�X@&@%�@%��@%�7@%Y�@%+�@%%F@%+@%�@$�@$�@$H@$@#�@#�K@#��@#RT@"҉@"��@"\�@"YK@"	@!��@!o @! \@ ��@ �U@ �z@ �Y@ m�@ ]d@ -�@�w@1�@�M@��@� @p;@@�@�@�@�.@�@�d@w2@X@/@��@m�@�@��@b�@W?@J#@;d@+@o@��@u%@a|@E�@0U@	@��@�=@c@T�@+@��@�p@�O@�4@�@~(@Q�@�@��@��@O@�@�@�]@�x@i�@5?@��@��@��@��@w2@�@�@j@7�@-�@�@��@|�@W?@F�@4�@�@S@�c@�<@��@~�@n�@0U@��@�H@k�@?}@q@�@�@|�@Q�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�I�A�IA�L0A�LdA�OBA�O�A�R�A�R�A�R�A�R�A�R A�T�A�U2A�VmA�X�A�^5A�^5A�\�A�^A�H�A�F�A�EmA�=<A�/�A�(XA�(�A�($A�)�A�<A�_A�a�A�m)A�sMA�w�A�|PA�|A�{JA�zxA�x�A�w�A�v`A�v`A�v+A�w2A�xlA�t�A�_�A��AƸA�d�A�7�A�(XA��A��QA�8�A½�A�j�A���A�D�A�zDA��eA��"A��jA��A�<A��A���A���A�4�A���A��A�`A�(A���A�wfA��*A�\)A� �Am�Ay�"Avm]Aot�AcZA\oiAZ�7AWQ�AT��ARv�AP�ANh
AL5�AH@�ACb�AAc A@�.A?JA<�AA9��A7��A7ƨA7U2A6�8A6r�A5��A3�A2$�A/��A.�A-��A-��A,�)A+�bA+*0A+�A)�A)7A(��A(YKA&��A&@A%A$,�A#]�A"��A#DgA#{A"A!�fA!�pA!^�A �MA ��A 1A�CAJ#A!�A��A��A�HAg8A��A��Az�ATaA��AbNA+A��A4�A��A�A�LA�AxlA�DAA_pA��Ay�AuA!�A�[A=qA�>A�DA�A��AN<A�AjA�mA~�AA�A�Ao A��AiDA'�A
�A
|�A	��A	Q�A	�A	Z�A	XyA�A�A=qAںAffA�-Ah�A�A�Aw�A�+A4A�MA�AA A��A+kA ��A ��A �A �}A O�A 8�@��
@��@�J@��c@���@�/�@��@���@��@�W�@�A�@��E@��@���@�m]@��@��P@�xl@�/�@�ی@��@�w�@�X@��@��
@�ی@�a�@�˒@��@��[@���@�@�R�@�[�@��A@��@�q@�YK@�u@�j@��m@鞄@��U@��#@��@�:�@彥@��@�F@�,�@�#�@�'�@�`�@��@��@��@��@��[@��@��@��@�n@�(@���@��@��@��@�c@�kQ@ݼ�@�j�@ܝI@�˒@�p�@�`B@��@�_�@ٱ[@�s@�҉@��@��@�_p@�F�@خ}@�Q�@�x�@ր�@�|@���@Ԓ�@�!-@�s@ի�@�|@�/�@�:�@�hs@�7L@�@�˒@Ԉ�@��@�9�@�c�@�(�@���@�5?@�(�@��@���@��@ϯ�@�2�@��@̗�@̔F@�_�@��@˙�@�T�@��@�7�@�4@ˠ'@��@�u�@�D�@��T@ɄM@�F@���@��?@ȣ�@�h
@��@�x�@�[W@��f@ƚ�@�~�@�V�@�4n@��K@�5�@��m@�z@�PH@�,=@�_@���@��6@ò�@Ë�@�V@��U@x@�/�@���@��@���@�u�@��5@��@���@���@�\�@��]@��@���@�s�@�6z@�,�@�&�@��@�@���@�~�@�>B@���@�X�@��@��b@�^5@�1@��6@��w@���@�N<@��@���@��@���@�m]@�:�@��@�Ov@���@�F@��@���@�q�@�%�@���@��X@��@��m@��@�u�@���@�^5@�@��'@��@�	@���@�iD@���@�GE@���@��k@�G�@�@��|@���@�u%@�4n@�{@���@�j@�Xy@�IR@���@�N�@�@���@�O@�;@��E@��e@�u%@�\�@�G@��j@��k@�_p@��@��p@���@�u�@�*�@��;@���@���@�"�@�B[@�}�@�.I@��f@���@��j@��_@���@�t�@�F@�%F@�S@��@��h@�r�@�@�@��@� �@�1�@�2�@�C�@���@�w2@�n/@�rG@�x@�P�@��@��x@�e�@� �@��)@��t@���@�u�@�@O@�!-@��6@�1�@���@�e,@��K@��r@�=q@��@��T@��
@�g�@���@��)@���@�4@���@��@��k@�%F@��,@���@�bN@�,=@��N@��@�a�@��@���@���@��@�g8@�M@��@���@�j@���@��1@�V�@��@��}@���@�Dg@��@��@��@�h�@�-�@��@���@��~@�x�@�w2@�e,@�A�@�1�@��@���@�ں@��_@�I�@��t@�!-@�ی@�U2@�e@��@��'@���@�RT@�V@���@�~�@�1'@��@��@���@�33@���@��@��\@�N�@�7@��@�خ@��F@�}�@�L�@��@��@���@��.@�h�@�!@��@���@��@��h@��~@��4@�|@�rG@�f�@�_p@�P�@�0�@��@��@�֡@��h@�W�@�*�@��@�m@˒@�w@�k@y�@�f@v`@qv@j�@E9@,�@~�"@~�1@~-@}��@}�-@}�~@}�@{��@{C�@{
=@z�@z�@z{�@zs�@z_�@y�Z@x�$@xXy@xK^@x2�@x�@w��@v��@vR�@v�@u��@uo @t��@t�O@t|�@tm�@t~@s�V@sH�@s!-@s�@r�M@r3�@q[W@p�D@o�@o�@o��@n�2@nJ�@mx�@l�)@l�@kb�@k$t@j��@j�@i�d@i�X@ic�@h��@h��@h1'@g�W@g�4@g�@f&�@ex�@e4@d��@d�@c�V@cZ�@c�@bl�@b{@a@a��@a��@azx@`֡@`q@`I�@_�q@_A�@^͟@]�@]x�@\��@\,=@[��@[J#@Z
�@Y�7@Y	l@X��@X�@W�$@Wn/@W8@V�<@V#:@U�@UQ�@T�j@TU2@T�@S��@Sb�@SK�@S@O@S&@R�@Rd�@R_@Q��@Q�@Q��@Q}�@Q=�@Q;@P�9@PQ�@O��@O��@OC@N�b@Nc @NTa@M��@M��@L�5@L��@Loi@L>B@L!@LM@L	�@Kݘ@K��@Ka@K/�@K�@J��@JTa@J:*@I�T@I�@H��@Hc�@G��@G�Q@GJ#@GC@G�@G�@F��@F�]@F��@F}V@Fn�@FZ�@FC�@F.�@F	@E�@E�'@D�@DZ@D�@C�@C�@C_p@B�c@B��@BZ�@A�S@A-w@A�@@��@@��@@�u@@z�@@2�@?�@?��@?~�@?C�@>��@>�L@>i�@=�@=5�@<�`@<��@<�O@<[�@;v`@;)_@:�H@:�<@:�}@:� @:n�@:3�@9��@94@8�@84n@7�F@7l�@7�@6�@6V@5��@5�@5��@5}�@5�@4�@4ی@4��@4�@4l"@4"h@3�@3��@3�@3W?@3!-@2�@2�!@1�o@0��@0�u@0*�@/�w@/��@/F�@.�"@.�@.�r@.6�@-��@-��@-<6@,��@,S�@,/�@+��@+qv@+O@+/�@+�@+@*�y@*�1@)�3@)��@)IR@(�5@(Ɇ@(�@(��@(z�@(?�@(G@'��@&�X@&@%�@%��@%�7@%Y�@%+�@%%F@%+@%�@$�@$�@$H@$@#�@#�K@#��@#RT@"҉@"��@"\�@"YK@"	@!��@!o @! \@ ��@ �U@ �z@ �Y@ m�@ ]d@ -�@�w@1�@�M@��@� @p;@@�@�@�@�.@�@�d@w2@X@/@��@m�@�@��@b�@W?@J#@;d@+@o@��@u%@a|@E�@0U@	@��@�=@c@T�@+@��@�p@�O@�4@�@~(@Q�@�@��@��@O@�@�@�]@�x@i�@5?@��@��@��@��@w2@�@�@j@7�@-�@�@��@|�@W?@F�@4�@�@S@�c@�<@��@~�@n�@0U@��@�H@k�@?}@q@�@�@|�@Q�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  BVBV9BV9BVmBV�BV�BV�BV�BVBVSBW
BV�BV�BV�BV�BVBU�BV�BV�BZ�B[�B[�B]~BaBd�Bf2Be�BgBmCBy�Bz�B.B��B��B�3B�B�mB��B�tB��B��B��B��B�7B��B��B��B	oOB	�B	�}B
VB
,�B
>�B
s�B
�%B
�.B
�1B
vFB
rGB
��B
��B
�B
|�B
uZB
�gB
�tB
�uB
�JB
x8B
pUB
dtB
>(B
+�B
�B
tB	�B	�	B	��B	�SB	��B	}"B	a�B	=VB	'B	�B	
XB�B�B�oB�>B��B՛BƨB��B�%B�B�eB�;B��B�B��B�%B�2B�B�B��B�RB��B��B��B	1B	/�B	6+B	=<B	=B	<�B	>�B	>B	?.B	G�B	J�B	L�B	I�B	O�B	`�B	nIB	m]B	q�B	z�B	�uB	�KB	�B	�aB	�B	�eB	��B	�BB	��B	��B	�0B	��B	�*B	�B	�4B	ÖB	�YB	ɠB	�B	��B	��B	�<B	�6B	�B	ЗB	�uB	�&B	�[B	��B	յB	רB	�qB	��B	ܬB	ܬB	��B	ޞB	�!B	�jB	�/B	�qB	�$B	ԕB	�B	��B	ؓB	�B	�B	ңB	ӏB	��B	�B	��B	�@B	��B	��B	ݘB	یB	�B	�&B	�HB	̈́B	�)B	��B	�=B	ɺB	�B	��B	�xB	�~B	�}B	�dB	��B	��B	ɠB	�^B	�6B	�jB	�PB	�B	�B	��B	͟B	��B	�B	�pB	�B	�pB	�B	�B	�B	�B	�/B	�sB	��B	ٴB	ބB	��B	�-B	��B	�'B	�B	�B	�sB	��B	�hB	�5B	�kB	چB	�dB	�B	�vB	��B	�B	�fB	�LB	�B	�B	�kB	��B	�B	�B	�qB	�0B	�$B	��B	�B	�*B	�]B	�wB	�/B	�B	��B	�B	�B	��B	�B	�;B	�OB	�B	�?B	�B	�nB	�B	��B	�ZB	�B	��B	�nB	�B	�'B	�B	��B	�DB	�B	�B	��B	�B	�B	�
B	�CB	�B	�GB	�hB	��B	�oB	�B	��B	�
B	�zB	�fB	��B	�[B	�B	�B	��B	��B	�cB	�UB	�GB	�>B
GB
�B	�cB	�<B	��B
�B
�B
�B
KB
�B
%B
gB
 �B	�B	��B
�B
�B
uB
AB
�B

�B
6B
jB
jB
PB
B
B
�B
�B
dB
0B
�B
�B
dB
xB
B

�B

�B
DB

�B

�B
)B
JB
B
�B
�B
�B
�B
xB
xB
�B
DB
DB
xB
xB
xB
�B
^B
B

�B

XB

=B
	�B
	�B
	�B
	�B
	�B

�B

�B

�B
DB
^B
DB
xB
^B
�B
�B
�B
�B
�B
0B
dB
�B
�B
"B
�B
�B
bB
.B
}B
 B
�B
�B
�B
�B
.B
NB
hB
B
:B
TB
�B
B
uB
�B
�B
�B
B
�B
 B
NB
HB
B
�B
�B
pB
�B
�B
jB
�B
BB
.B
 B
 B
�B
FB
aB
�B
�B
@B
�B
HB
\B
(B
�B
�B
�B
�B
\B
�B
B
�B
�B
.B
.B
HB
bB
.B
HB
bB
}B
HB
.B
B
�B
VB
�B
6B
�B
~B
�B
�B

�B
DB
xB
B
0B
�B
�B
"B
�B
(B
NB
�B
�B
gB
2B
B
?B
B
�B
_B
�B
�B
=B
=B
WB
=B
qB
�B
WB
�B
dB
�B
~B
xB
�B
=B
qB
B
]B
�B
IB
OB
�B
dB
�B
�B
�B
�B
�B
B
OB
jB
�B
�B
;B
;B
pB
�B
 �B
"B
"B
"NB
"�B
"�B
"4B
"4B
"�B
#:B
#�B
#�B
$�B
%,B
%zB
%�B
%�B
%�B
%�B
&fB
&fB
&�B
&�B
&�B
&�B
&�B
'8B
'8B
'8B
'mB
'mB
(�B
)B
(�B
)�B
)�B
*KB
*�B
*B
+B
+6B
+6B
,"B
,WB
,�B
,qB
-�B
-�B
-�B
.}B
.}B
/ B
.�B
.�B
.�B
.�B
/ B
/OB
/�B
/�B
/�B
0B
0;B
0oB
0�B
0�B
1'B
1'B
1'B
1AB
1'B
1[B
1[B
1[B
1�B
1�B
1�B
1�B
1�B
1�B
2�B
2�B
2�B
2�B
2�B
2�B
2�B
2�B
2�B
3B
3B
33B
33B
33B
33B
3�B
3�B
3�B
3�B
3�B
4B
5B
5�B
5tB
5�B
5�B
5�B
5�B
5�B
6+B
7�B
7�B
7�B
7�B
7�B
8lB
8�B
8�B
8�B
8�B
8�B
8�B
9>B
9XB
9rB
9�B
9�B
9�B
9�B
9�B
9�B
:^B
:�B
;B
;�B
<jB
=<B
=�B
>]B
>�B
?.B
?�B
@ B
@B
@iB
@�B
@�B
@�B
@�B
AB
A;B
AoB
A�B
A�B
B'B
B�B
CB
C-B
C�B
C�B
C�B
C�B
DB
D�B
D�B
EB
D�B
D�B
D�B
E�B
E�B
E�B
E�B
FB
FYB
F�B
GB
GEB
G�B
G�B
G�B
H�B
IB
IlB
I�B
I�B
I�B
J	B
J#B
JrB
J�B
KB
K)B
K�B
K�B
K�B
LdB
L~B
L~B
L�B
L�B
L�B
M6B
MPB
MPB
MPB
MjB
M�B
M�B
M�B
NB
NVB
N�B
N�B
O�B
O�B
O�B
O�B
P�B
P�B
Q�B
Q�B
Q�B
R B
R:B
R:B
R:B
RTB
RTB
R�B
R�B
R�B
S&B
S&B
S&B
SuB
T,B
T,B
T{B
T�B
T�B
UgB
UgB
UgB
U�B
UgB
U�B
U�B
U�B
U�B
U�B
U�B
U�B
U�B
VB
VB
W$B
V�B
W$B
W$B
W?B
W�B
W�B
W�B
W�B
X�B
X�B
X�B
Y1B
Y1B
Y1B
Y1B
YeB
YB
Y�B
Y�B
Y�B
Y�B
ZB
ZB
Z�B
[	B
[#B
[	B
[	B
[WB
[�B
\B
\)B
\]B
\]B
\]B
\�B
\�B
\�B
]B
]IB
]�B
]�B
]�B
^B
^OB
^�B
^�B
^�B
_B
_;B
_�B
_�B
_�B
_�B
_�B
_�B
`'B
`�B
`�B
`�B
aB
aHB
a-B
aHB
a�B
b�B
bhB
b�B
b�B
b�B
c B
c B
c:B
c�B
c�B
d&B
d�B
d�B
d�B
d�B
d�B
e`B
e,B
e`B
e`B
e�B
e�B
e�B
e�B
f�B
f�B
gB
g�B
g�B
g�B
g�B
g�B
h$B
hsB
h�B
i�B
jeB
jB
j�B
j�B
k6B
kkB
kkB
kkB
k�B
k�B
lB
lqB
l�B
l�B
mB
m]B
mwB
ncB
n�B
n�B
n�B
o B
o5B
o�B
o�B
o�B
pB
p!B
p;B
p;B
pUB
poB
p�B
qAB
qvB
q�B
q�B
q�B
rB
r-B
rGB
rGB
rGB
r|B
r�B
r�B
r�B
s3B
shB
s�B
tB
t9B
tTB
tnB
tnB
tTB
t�B
t�B
uB
uB
u?B
uZB
utB
u�B
u�B
u�B
vB
vFB
v`B
v`B
vzB
v�B
vzB
v�B
v�B
w2B
w2B
wfB
w�B
w�B
w�B
x8B
x8B
xRB
xlB
x�B
x�B
x�B
x�B
x�B
y�B
y�B
y�B
zB
y�B
z^B
z�B
z�B
z�B
z�B
z�B
{B
{B
z�B
{JB
{dB
{dB
{dB
{�B
{�B
|B
|PB
|PB
|jB
|�B
|�B
}B
}V1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  BVBV9BV9BVmBV�BV�BV�BV�BVBVSBW
BV�BV�BV�BV�BVBU�BV�BV�BZ�B[�B[�B]~BaBd�Bf2Be�BgBmCBy�Bz�B.B��B��B�3B�B�mB��B�tB��B��B��B��B�7B��B��B��B	oOB	�B	�}B
VB
,�B
>�B
s�B
�%B
�.B
�1B
vFB
rGB
��B
��B
�B
|�B
uZB
�gB
�tB
�uB
�JB
x8B
pUB
dtB
>(B
+�B
�B
tB	�B	�	B	��B	�SB	��B	}"B	a�B	=VB	'B	�B	
XB�B�B�oB�>B��B՛BƨB��B�%B�B�eB�;B��B�B��B�%B�2B�B�B��B�RB��B��B��B	1B	/�B	6+B	=<B	=B	<�B	>�B	>B	?.B	G�B	J�B	L�B	I�B	O�B	`�B	nIB	m]B	q�B	z�B	�uB	�KB	�B	�aB	�B	�eB	��B	�BB	��B	��B	�0B	��B	�*B	�B	�4B	ÖB	�YB	ɠB	�B	��B	��B	�<B	�6B	�B	ЗB	�uB	�&B	�[B	��B	յB	רB	�qB	��B	ܬB	ܬB	��B	ޞB	�!B	�jB	�/B	�qB	�$B	ԕB	�B	��B	ؓB	�B	�B	ңB	ӏB	��B	�B	��B	�@B	��B	��B	ݘB	یB	�B	�&B	�HB	̈́B	�)B	��B	�=B	ɺB	�B	��B	�xB	�~B	�}B	�dB	��B	��B	ɠB	�^B	�6B	�jB	�PB	�B	�B	��B	͟B	��B	�B	�pB	�B	�pB	�B	�B	�B	�B	�/B	�sB	��B	ٴB	ބB	��B	�-B	��B	�'B	�B	�B	�sB	��B	�hB	�5B	�kB	چB	�dB	�B	�vB	��B	�B	�fB	�LB	�B	�B	�kB	��B	�B	�B	�qB	�0B	�$B	��B	�B	�*B	�]B	�wB	�/B	�B	��B	�B	�B	��B	�B	�;B	�OB	�B	�?B	�B	�nB	�B	��B	�ZB	�B	��B	�nB	�B	�'B	�B	��B	�DB	�B	�B	��B	�B	�B	�
B	�CB	�B	�GB	�hB	��B	�oB	�B	��B	�
B	�zB	�fB	��B	�[B	�B	�B	��B	��B	�cB	�UB	�GB	�>B
GB
�B	�cB	�<B	��B
�B
�B
�B
KB
�B
%B
gB
 �B	�B	��B
�B
�B
uB
AB
�B

�B
6B
jB
jB
PB
B
B
�B
�B
dB
0B
�B
�B
dB
xB
B

�B

�B
DB

�B

�B
)B
JB
B
�B
�B
�B
�B
xB
xB
�B
DB
DB
xB
xB
xB
�B
^B
B

�B

XB

=B
	�B
	�B
	�B
	�B
	�B

�B

�B

�B
DB
^B
DB
xB
^B
�B
�B
�B
�B
�B
0B
dB
�B
�B
"B
�B
�B
bB
.B
}B
 B
�B
�B
�B
�B
.B
NB
hB
B
:B
TB
�B
B
uB
�B
�B
�B
B
�B
 B
NB
HB
B
�B
�B
pB
�B
�B
jB
�B
BB
.B
 B
 B
�B
FB
aB
�B
�B
@B
�B
HB
\B
(B
�B
�B
�B
�B
\B
�B
B
�B
�B
.B
.B
HB
bB
.B
HB
bB
}B
HB
.B
B
�B
VB
�B
6B
�B
~B
�B
�B

�B
DB
xB
B
0B
�B
�B
"B
�B
(B
NB
�B
�B
gB
2B
B
?B
B
�B
_B
�B
�B
=B
=B
WB
=B
qB
�B
WB
�B
dB
�B
~B
xB
�B
=B
qB
B
]B
�B
IB
OB
�B
dB
�B
�B
�B
�B
�B
B
OB
jB
�B
�B
;B
;B
pB
�B
 �B
"B
"B
"NB
"�B
"�B
"4B
"4B
"�B
#:B
#�B
#�B
$�B
%,B
%zB
%�B
%�B
%�B
%�B
&fB
&fB
&�B
&�B
&�B
&�B
&�B
'8B
'8B
'8B
'mB
'mB
(�B
)B
(�B
)�B
)�B
*KB
*�B
*B
+B
+6B
+6B
,"B
,WB
,�B
,qB
-�B
-�B
-�B
.}B
.}B
/ B
.�B
.�B
.�B
.�B
/ B
/OB
/�B
/�B
/�B
0B
0;B
0oB
0�B
0�B
1'B
1'B
1'B
1AB
1'B
1[B
1[B
1[B
1�B
1�B
1�B
1�B
1�B
1�B
2�B
2�B
2�B
2�B
2�B
2�B
2�B
2�B
2�B
3B
3B
33B
33B
33B
33B
3�B
3�B
3�B
3�B
3�B
4B
5B
5�B
5tB
5�B
5�B
5�B
5�B
5�B
6+B
7�B
7�B
7�B
7�B
7�B
8lB
8�B
8�B
8�B
8�B
8�B
8�B
9>B
9XB
9rB
9�B
9�B
9�B
9�B
9�B
9�B
:^B
:�B
;B
;�B
<jB
=<B
=�B
>]B
>�B
?.B
?�B
@ B
@B
@iB
@�B
@�B
@�B
@�B
AB
A;B
AoB
A�B
A�B
B'B
B�B
CB
C-B
C�B
C�B
C�B
C�B
DB
D�B
D�B
EB
D�B
D�B
D�B
E�B
E�B
E�B
E�B
FB
FYB
F�B
GB
GEB
G�B
G�B
G�B
H�B
IB
IlB
I�B
I�B
I�B
J	B
J#B
JrB
J�B
KB
K)B
K�B
K�B
K�B
LdB
L~B
L~B
L�B
L�B
L�B
M6B
MPB
MPB
MPB
MjB
M�B
M�B
M�B
NB
NVB
N�B
N�B
O�B
O�B
O�B
O�B
P�B
P�B
Q�B
Q�B
Q�B
R B
R:B
R:B
R:B
RTB
RTB
R�B
R�B
R�B
S&B
S&B
S&B
SuB
T,B
T,B
T{B
T�B
T�B
UgB
UgB
UgB
U�B
UgB
U�B
U�B
U�B
U�B
U�B
U�B
U�B
U�B
VB
VB
W$B
V�B
W$B
W$B
W?B
W�B
W�B
W�B
W�B
X�B
X�B
X�B
Y1B
Y1B
Y1B
Y1B
YeB
YB
Y�B
Y�B
Y�B
Y�B
ZB
ZB
Z�B
[	B
[#B
[	B
[	B
[WB
[�B
\B
\)B
\]B
\]B
\]B
\�B
\�B
\�B
]B
]IB
]�B
]�B
]�B
^B
^OB
^�B
^�B
^�B
_B
_;B
_�B
_�B
_�B
_�B
_�B
_�B
`'B
`�B
`�B
`�B
aB
aHB
a-B
aHB
a�B
b�B
bhB
b�B
b�B
b�B
c B
c B
c:B
c�B
c�B
d&B
d�B
d�B
d�B
d�B
d�B
e`B
e,B
e`B
e`B
e�B
e�B
e�B
e�B
f�B
f�B
gB
g�B
g�B
g�B
g�B
g�B
h$B
hsB
h�B
i�B
jeB
jB
j�B
j�B
k6B
kkB
kkB
kkB
k�B
k�B
lB
lqB
l�B
l�B
mB
m]B
mwB
ncB
n�B
n�B
n�B
o B
o5B
o�B
o�B
o�B
pB
p!B
p;B
p;B
pUB
poB
p�B
qAB
qvB
q�B
q�B
q�B
rB
r-B
rGB
rGB
rGB
r|B
r�B
r�B
r�B
s3B
shB
s�B
tB
t9B
tTB
tnB
tnB
tTB
t�B
t�B
uB
uB
u?B
uZB
utB
u�B
u�B
u�B
vB
vFB
v`B
v`B
vzB
v�B
vzB
v�B
v�B
w2B
w2B
wfB
w�B
w�B
w�B
x8B
x8B
xRB
xlB
x�B
x�B
x�B
x�B
x�B
y�B
y�B
y�B
zB
y�B
z^B
z�B
z�B
z�B
z�B
z�B
{B
{B
z�B
{JB
{dB
{dB
{dB
{�B
{�B
|B
|PB
|PB
|jB
|�B
|�B
}B
}V1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104859  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604172938  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604172938  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604172938                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605022946  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605022946  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610131507                      G�O�G�O�G�O�                