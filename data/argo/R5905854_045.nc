CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:52:35Z creation;2022-06-04T17:52:35Z conversion to V3.1      
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
_FillValue                 �  I<   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M,   PRES_ADJUSTED_QC         
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
_FillValue                 �  �@   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �0   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
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
resolution        :�o     �  �D   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ޼   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �    HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �$   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �(   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �h   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �x   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �|   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604175235  20220610141505  5905854                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               -A   JA                                  2B  A   APEX                            8422                            2.11.2                          846 @��(�1   @���\(�@0�\(��ci�^5?}1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @33@�  @�  A   A   A@  A`  A�  A�  A���A�  A�  A�  A�  A�  B   B  B  B  B��B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�33B�  B�ffB���B���B���B�  B�  B�  B�  B�  B�  B�  B�  BЙ�B�33Bי�B�  B�  B�  B�  B�  B�  B�  B���B�  C L�C�C�3C  C�fC	�fC  C  CffC�fC  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:�C<33C=�fC?�fCB  CD  CF  CH  CJ  CL  CN  CO�fCR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch�Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DNfDN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D{��D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�3D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�
@�Q�@�Q�A (�A (�A@(�A`(�A�{A�{A��HA�{A�{A�{A�{A�{B 
=B
=B
=B
=B��B(
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
=B�B�B�B�B�B�B�8RB�B�k�B���B���B���B�B�B�B�B�B�B�B�BО�B�8RBמ�B�B�B�B�B�B�B�B���B�C O\C)C��C�C��C	��C�C�Ch�C��C�C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:)C<5�C=��C?��CB�CD�CF�CH�CJ�CL�CN�CO��CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch)Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�C�C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�C�HC�HC�HC�HC�HC�HC�HC�HC�HD  �D ��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D	 �D	��D
 �D
��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D  �D ��D! �D!��D" �D"��D# �D#��D$ �D$��D% �D%��D& �D&��D' �D'��D( �D(��D) �D)��D* �D*��D+ �D+��D, �D,��D- �D-��D. �D.��D/ �D/��D0 �D0��D1 �D1��D2 �D2��D3 �D3��D4 �D4��D5 �D5��D6 �D6��D7 �D7��D8 �D8��D9 �D9��D: �D:��D; �D;��D< �D<��D= �D=��D> �D>��D? �D?��D@ �D@��DA �DA��DB �DB��DC �DC��DD �DD��DE �DE��DF �DF��DG �DG��DH �DH��DI �DI��DJ �DJ��DK �DK��DL �DL��DM �DM��DN
DN��DO �DO��DP �DP��DQ �DQ��DR �DR��DS �DS��DT �DT��DU �DU��DV �DV��DW �DW��DX �DX��DY �DY��DZ �DZ��D[ �D[��D\ �D\��D] �D]��D^ �D^��D_ �D_��D` �D`��Da �Da��Db �Db��Dc �Dc��Dd �Dd��De �De��Df �Df��Dg �Dg��Dh �Dh��Di �Di��Dj �Dj��Dk �Dk��Dl �Dl��Dm �Dm��Dn �Dn��Do �Do��Dp �Dp��Dq �Dq��Dr �Dr��Ds �Ds��Dt �Dt��Du �Du��Dv �Dv��Dw �Dw��Dx �Dx��Dy �Dy��Dz �Dz��D{ �D{��D{�>D|��D} �D}��D~ �D~��D �D��D� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�=D��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RDRD��RD� RD�@RDÀRD��RD� RD�@RDĀRD��RD� RD�@RDŀRD��RD� RD�@RDƀRD��RD� RD�@RDǀRD��RD��D�@RDȀRD��RD� RD�@RDɀRD��RD� RD�@RDʀRD��RD� RD�@RDˀRD��RD� RD�@RD̀RD��RD� RD�@RD̀RD��RD� RD�@RD΀RD��RD� RD�@RDπRD��RD� RD�@RDЀRD��RD� RD�@RDрRD��RD� RD�@RDҀRD��RD� RD�@RDӀRD��RD� RD�@RDԀRD��RD� RD�@RDՀRD��RD� RD�@RDրRD��RD� RD�@RD׀RD��RD� RD�@RD؀RD��RD� RD�@RDـRD��RD� RD�@RDڀRD��RD� RD�@RDۀRD��RD� RD�@RD܀RD��RD� RD�@RD݀RD��RD� RD�@RDހRD��RD� RD�@RD߀RD��RD� RD�@RD��RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD��RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��vA��A��|A��A���A��A���A��A���A�uA��A�A��A��A�%A��A�A��]A���A���AʵA�:*A���Aɵ�Aɣ:Aɕ�A�zA�v`A�v+A�r�A�V9A�7�A�:�A�<�A�G�A�C�A��AǤ�AƠ�A�&A�Y�A��A�iA��A¿A���A�kQA��|A�qAA�/�A�S�A���A�jA���A��A���A���A�[�A��dA�D�A��A��PA�A�
	A���A��?A���A��4A�p;A���A�XyA�!�A�'A��dA��A���A��TA�֡A��dA�OA��2A�@A�(�A�B'A�uA���A���A��A�x�A���A��0A�7�A���A��aAz��Av�[At8Ao.�Ah��Af(�A^˒A[��AY1AW+AO��AK�AAJo AI�AAF�0AC1�AA8A@�NA? \A<��A:��A9��A9�fA8��A7�.A7��A5{�A3�DA2�A0J�A.u�A-��A-W�A-��A.�A,��A+�A)�"A)v�A(�3A(��A(_A'�'A%c�A%L�A&��A&�A&7LA%�)A%A%�A%)_A$��A#��A#R�A#�A"�A"|�A"#:A!m�A�AhsAc�Ak�A�"A<6A�yA��A \A�XA�kA�HA�YA��AJ�AN<A��A��A�UA��AbA��AHA��Az�AbNA[�A/�A�A iAXyA4nA	lA�A��Ag8A�Am�A�+A�uA@�AĜA=A�AF�A{A
�	A
o�A
�A	iDA��A4�A�A��AA�A&�A�A �A��A��A�A�sAl"A&�A�rA�}AOA��Ac�A��AhsA!�A�"A�A��A�oAZ�A ��A 0�@�!-@��f@���@��@��@�M�@��@���@�-w@�ff@���@�;d@��|@��z@�_�@��@�(�@�@�=@���@�m�@�@��H@�&�@��@�4n@��@�_@�� @�Q�@���@��@�1@���@�_p@� i@�Ɇ@�C-@��@�@���@�+k@��@��@�Ɇ@橓@�>B@��o@��@�o@�?@�y�@�xl@��a@�$@�.I@�H@�RT@���@�҉@���@޽<@޳h@�3�@���@ݡ�@݊�@�F�@�+@ܤ�@�-�@��@���@�S�@�ȴ@�s�@ٶF@٣n@�U�@���@دO@׺^@���@�[�@�Z�@�@��@�1@�g�@Ҵ9@�x@Ѳ-@�@O@��g@�>B@��Z@��A@Ѽ@�K�@ІY@�C�@�{@��@ς�@���@�C-@͠�@̛�@��@��@��@ɜ�@���@ț�@��9@ǉ7@�rG@�#�@��s@�"h@ż@Ŏ"@�_p@�(@�ѷ@ĆY@�.�@�خ@�c�@�j@�(�@���@�a�@���@���@�I�@�@���@���@�"�@�s�@��@���@���@�iD@���@�(�@���@�Z�@�PH@�1'@�M@��@�Dg@�-w@��@�"h@��@��1@�kQ@��@�q@�j�@�t�@���@�� @�w�@�/�@��@��@�<6@�!-@���@�Ĝ@���@�U2@���@�J#@�ں@�r�@���@���@��}@���@���@�y>@�ff@�V�@�M@�I�@�E�@�"h@���@�4@�{�@�@�@�t�@��2@��@��_@�C�@��N@�7L@��A@�H@��z@�?}@�ں@�z@�U2@��m@�4�@���@�e@���@�C�@���@��F@��o@���@���@�r�@�c�@�S�@��}@�`B@�#�@��,@��u@���@�Ft@��=@�@�ȴ@���@��@���@�n�@�V@�U2@�I�@�M@��@��n@��M@�c�@�\�@�B�@��@��j@�_@�A�@��Z@���@�IR@��@��F@�{�@�w�@�_�@�	�@��@��f@�T�@�1�@��@��@��@��H@�y>@���@���@�`B@�4@���@��6@���@�?�@� �@�  @��&@��"@�8�@��@�~(@���@��	@�s�@�m�@�:�@���@���@�S&@�0�@�S@�҉@���@�oi@��.@���@�
=@���@���@�e�@�?@�b@��Z@���@��N@��[@�x@�A�@��@��@�n�@��@�y�@�W?@�H�@�.I@�ی@�D�@�/�@�G@��W@��C@�?}@� \@��@���@��@���@�q@�.�@���@��{@�33@�o@��@�Ĝ@��O@�YK@���@��@���@�#�@���@�� @�v�@�Ov@�e@�J�@��[@�� @�c�@�Q@�C-@�$@��@���@�}�@�IR@��@���@���@�s�@�c�@�@�@�G@���@�e,@�Q�@�@���@��Y@�q@�c�@�Ta@�)�@�~@�{@��@�b@�a@~��@~3�@}ϫ@}��@}T�@|�@|�@|V�@|�@{�@{��@{�:@{J#@{�@z��@y�N@y}�@y�@x��@x�Y@xC-@w�]@w�V@w�@v�s@v�@v��@vV@u�@u�M@uY�@u/@t��@tm�@t1@s�@s�@r��@r��@r��@r�+@ri�@r0U@q�#@q�-@q\�@p��@p�9@p�@py>@p9X@o�@o��@o�@n��@nl�@m��@m�7@m4@l�$@lh�@lK^@l2�@l7@k�*@k�@j��@jQ@i��@iw2@i:�@h�P@h�p@h�o@hD�@h�@g�@g�0@g$t@fxl@f�@e��@d��@dC-@c��@c��@c�f@cMj@b�2@b�@bZ�@a�@azx@`u�@`]d@`[�@``�@`V�@`�@_��@_�K@_\)@^��@^h
@]��@]<6@]�@\�p@\[�@[�@[�0@[�@Zں@Zn�@Y��@Y�@X,=@X@W��@W�4@W@O@WY@V��@V�@V�@V�@U��@U/@T��@T<�@Sb�@S@R��@R��@Rd�@Q�.@Q��@Qc�@QQ�@QV@P�@PM@O�6@O��@O��@OH�@N��@Nc @M��@M�M@M��@Ms�@MS&@MN<@M?}@M-w@M�@L��@Lѷ@L��@L�D@L>B@L�@Ky�@K/�@J�@J��@JB[@I�@IY�@IL�@IDg@I=�@I+�@I@Hy>@G��@G� @G��@G�g@G�K@Gb�@GC@F�@F�@F��@FQ@F&�@F�@E�@E��@EJ�@D�@D|�@D�@C�Q@C��@C��@C,�@B�@B��@B�L@Bq�@BH�@A�@A�@A��@A�=@AN<@@��@@Z@@*�@?��@?=@>�H@>}V@>=q@=��@=��@=c�@=�@<�I@<b@;�	@;Mj@;�@:�X@:~�@:	@9�@9�^@9��@9�@9[W@9F@8��@8��@8u�@8H@8(�@7�
@7j�@6�y@6�+@6Ov@6O@5�@5rG@5%F@5�@5	l@4�K@4ѷ@4��@3��@2��@2�+@2n�@25?@2 �@1��@1�@1��@1�@1�@1�d@1��@1�h@1rG@1e,@17L@0�p@0I�@/�;@/��@/�Q@/��@/˒@/o�@. �@-�@-�'@-�@,*�@,1@,G@+�@+��@+j�@+\)@+Mj@+�@*�2@*��@*:*@)�~@)Y�@)Dg@)B�@)A @)/@)%F@(�|@(�D@(?�@'�@'�*@'C�@&��@&�1@&W�@&GE@&=q@&6�@&�@%�S@%o @%O�@%�@$�/@$PH@#��@#�@#b�@#@O@#33@#,�@#"�@#�@#
=@#S@"��@"ߤ@"��@"�r@"l�@"#:@!�o@!p�@ ��@ ��@ A�@ �@�@a@
=@_�@@�@hs@@@��@�@Z@�@�[@l�@(@��@��@�@?@�Z@�)@�-@w2@�@�/@l"@ݘ@{J@O@ i@i�@
�@�H@�=@f�@B�@�@��@~(@Ft@	�@�@�V@l�@_p@A�@811111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��vA��A��|A��A���A��A���A��A���A�uA��A�A��A��A�%A��A�A��]A���A���AʵA�:*A���Aɵ�Aɣ:Aɕ�A�zA�v`A�v+A�r�A�V9A�7�A�:�A�<�A�G�A�C�A��AǤ�AƠ�A�&A�Y�A��A�iA��A¿A���A�kQA��|A�qAA�/�A�S�A���A�jA���A��A���A���A�[�A��dA�D�A��A��PA�A�
	A���A��?A���A��4A�p;A���A�XyA�!�A�'A��dA��A���A��TA�֡A��dA�OA��2A�@A�(�A�B'A�uA���A���A��A�x�A���A��0A�7�A���A��aAz��Av�[At8Ao.�Ah��Af(�A^˒A[��AY1AW+AO��AK�AAJo AI�AAF�0AC1�AA8A@�NA? \A<��A:��A9��A9�fA8��A7�.A7��A5{�A3�DA2�A0J�A.u�A-��A-W�A-��A.�A,��A+�A)�"A)v�A(�3A(��A(_A'�'A%c�A%L�A&��A&�A&7LA%�)A%A%�A%)_A$��A#��A#R�A#�A"�A"|�A"#:A!m�A�AhsAc�Ak�A�"A<6A�yA��A \A�XA�kA�HA�YA��AJ�AN<A��A��A�UA��AbA��AHA��Az�AbNA[�A/�A�A iAXyA4nA	lA�A��Ag8A�Am�A�+A�uA@�AĜA=A�AF�A{A
�	A
o�A
�A	iDA��A4�A�A��AA�A&�A�A �A��A��A�A�sAl"A&�A�rA�}AOA��Ac�A��AhsA!�A�"A�A��A�oAZ�A ��A 0�@�!-@��f@���@��@��@�M�@��@���@�-w@�ff@���@�;d@��|@��z@�_�@��@�(�@�@�=@���@�m�@�@��H@�&�@��@�4n@��@�_@�� @�Q�@���@��@�1@���@�_p@� i@�Ɇ@�C-@��@�@���@�+k@��@��@�Ɇ@橓@�>B@��o@��@�o@�?@�y�@�xl@��a@�$@�.I@�H@�RT@���@�҉@���@޽<@޳h@�3�@���@ݡ�@݊�@�F�@�+@ܤ�@�-�@��@���@�S�@�ȴ@�s�@ٶF@٣n@�U�@���@دO@׺^@���@�[�@�Z�@�@��@�1@�g�@Ҵ9@�x@Ѳ-@�@O@��g@�>B@��Z@��A@Ѽ@�K�@ІY@�C�@�{@��@ς�@���@�C-@͠�@̛�@��@��@��@ɜ�@���@ț�@��9@ǉ7@�rG@�#�@��s@�"h@ż@Ŏ"@�_p@�(@�ѷ@ĆY@�.�@�خ@�c�@�j@�(�@���@�a�@���@���@�I�@�@���@���@�"�@�s�@��@���@���@�iD@���@�(�@���@�Z�@�PH@�1'@�M@��@�Dg@�-w@��@�"h@��@��1@�kQ@��@�q@�j�@�t�@���@�� @�w�@�/�@��@��@�<6@�!-@���@�Ĝ@���@�U2@���@�J#@�ں@�r�@���@���@��}@���@���@�y>@�ff@�V�@�M@�I�@�E�@�"h@���@�4@�{�@�@�@�t�@��2@��@��_@�C�@��N@�7L@��A@�H@��z@�?}@�ں@�z@�U2@��m@�4�@���@�e@���@�C�@���@��F@��o@���@���@�r�@�c�@�S�@��}@�`B@�#�@��,@��u@���@�Ft@��=@�@�ȴ@���@��@���@�n�@�V@�U2@�I�@�M@��@��n@��M@�c�@�\�@�B�@��@��j@�_@�A�@��Z@���@�IR@��@��F@�{�@�w�@�_�@�	�@��@��f@�T�@�1�@��@��@��@��H@�y>@���@���@�`B@�4@���@��6@���@�?�@� �@�  @��&@��"@�8�@��@�~(@���@��	@�s�@�m�@�:�@���@���@�S&@�0�@�S@�҉@���@�oi@��.@���@�
=@���@���@�e�@�?@�b@��Z@���@��N@��[@�x@�A�@��@��@�n�@��@�y�@�W?@�H�@�.I@�ی@�D�@�/�@�G@��W@��C@�?}@� \@��@���@��@���@�q@�.�@���@��{@�33@�o@��@�Ĝ@��O@�YK@���@��@���@�#�@���@�� @�v�@�Ov@�e@�J�@��[@�� @�c�@�Q@�C-@�$@��@���@�}�@�IR@��@���@���@�s�@�c�@�@�@�G@���@�e,@�Q�@�@���@��Y@�q@�c�@�Ta@�)�@�~@�{@��@�b@�a@~��@~3�@}ϫ@}��@}T�@|�@|�@|V�@|�@{�@{��@{�:@{J#@{�@z��@y�N@y}�@y�@x��@x�Y@xC-@w�]@w�V@w�@v�s@v�@v��@vV@u�@u�M@uY�@u/@t��@tm�@t1@s�@s�@r��@r��@r��@r�+@ri�@r0U@q�#@q�-@q\�@p��@p�9@p�@py>@p9X@o�@o��@o�@n��@nl�@m��@m�7@m4@l�$@lh�@lK^@l2�@l7@k�*@k�@j��@jQ@i��@iw2@i:�@h�P@h�p@h�o@hD�@h�@g�@g�0@g$t@fxl@f�@e��@d��@dC-@c��@c��@c�f@cMj@b�2@b�@bZ�@a�@azx@`u�@`]d@`[�@``�@`V�@`�@_��@_�K@_\)@^��@^h
@]��@]<6@]�@\�p@\[�@[�@[�0@[�@Zں@Zn�@Y��@Y�@X,=@X@W��@W�4@W@O@WY@V��@V�@V�@V�@U��@U/@T��@T<�@Sb�@S@R��@R��@Rd�@Q�.@Q��@Qc�@QQ�@QV@P�@PM@O�6@O��@O��@OH�@N��@Nc @M��@M�M@M��@Ms�@MS&@MN<@M?}@M-w@M�@L��@Lѷ@L��@L�D@L>B@L�@Ky�@K/�@J�@J��@JB[@I�@IY�@IL�@IDg@I=�@I+�@I@Hy>@G��@G� @G��@G�g@G�K@Gb�@GC@F�@F�@F��@FQ@F&�@F�@E�@E��@EJ�@D�@D|�@D�@C�Q@C��@C��@C,�@B�@B��@B�L@Bq�@BH�@A�@A�@A��@A�=@AN<@@��@@Z@@*�@?��@?=@>�H@>}V@>=q@=��@=��@=c�@=�@<�I@<b@;�	@;Mj@;�@:�X@:~�@:	@9�@9�^@9��@9�@9[W@9F@8��@8��@8u�@8H@8(�@7�
@7j�@6�y@6�+@6Ov@6O@5�@5rG@5%F@5�@5	l@4�K@4ѷ@4��@3��@2��@2�+@2n�@25?@2 �@1��@1�@1��@1�@1�@1�d@1��@1�h@1rG@1e,@17L@0�p@0I�@/�;@/��@/�Q@/��@/˒@/o�@. �@-�@-�'@-�@,*�@,1@,G@+�@+��@+j�@+\)@+Mj@+�@*�2@*��@*:*@)�~@)Y�@)Dg@)B�@)A @)/@)%F@(�|@(�D@(?�@'�@'�*@'C�@&��@&�1@&W�@&GE@&=q@&6�@&�@%�S@%o @%O�@%�@$�/@$PH@#��@#�@#b�@#@O@#33@#,�@#"�@#�@#
=@#S@"��@"ߤ@"��@"�r@"l�@"#:@!�o@!p�@ ��@ ��@ A�@ �@�@a@
=@_�@@�@hs@@@��@�@Z@�@�[@l�@(@��@��@�@?@�Z@�)@�-@w2@�@�/@l"@ݘ@{J@O@ i@i�@
�@�H@�=@f�@B�@�@��@~(@Ft@	�@�@�V@l�@_p@A�@811111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B��B��B��B�[B�B�AB�uB��B�-B�B�aB�-B�GB�B�aB�{B��B��B��By>BT�BTFBW�B[qB\]B]B`'Ba�BbhBe�BvzBx�B�B��B��B�B��Bm�Bm�B.B�QB��B�B	=�B	kQB	�B	}�B	��B	خB	��B	��B
�B
!�B
 B
.�B
?.B
j�B
��B
��B
�B
�}B
�dB
��B
�GB
�B
�=B
P}B
B
�B
!bB
8B
U�B
l�B
|jB
�B
�B
��B
LdB
@OB
9�B
8�B
NVB
��B[B�B
�!B
��B
��B
�B
�B
��B
jB
KB
�B	��B	��B	�B	��B	bNB	MB	"�B		�B��B�B�/B��B�}B�jB��B�#B�MB�bB��B�ZB	�B	vB	�B	bB	:B	NB	�B	�B	DB	{B	�B	-)B	7fB	N�B	jB	kB	nB	n�B	rGB	x�B	��B	�oB	�B	|�B	�@B	��B	�PB	�&B	�uB	ԯB	�9B	�OB	�B	�
B	�B	�B	�B	�@B	�B	�B	�EB	�aB	�,B	�7B	��B	�B	��B	�B	��B	��B	�B	�=B	�IB	��B	�dB	�{B	��B	�B	چB	��B	�$B	�B	��B	�KB	��B	��B	ּB	��B	��B	��B	�+B	׍B	��B	خB	�OB	�4B	��B	޸B	��B	�IB	�B	�xB	ܬB	�IB	��B	�jB	ޞB	�;B	޸B	�!B	�B	�jB	�jB	޸B	�B	ޞB	ޞB	ޞB	ބB	ބB	�jB	�B	�OB	�B	��B	�5B	ޞB	�5B	�jB	��B	�B	ބB	�!B	��B	�]B	��B	�#B	�WB	�	B	��B	��B	�$B	�mB	�B	��B	�{B	�,B	�B	�[B	҉B	�:B	��B	��B	�B	�[B	ӏB	ӏB	��B	ԕB	յB	�+B	�1B	�B	�yB	�YB	��B	�eB	ٴB	�kB	ٴB	�kB	چB	ںB	��B	چB	��B	��B	ܒB	ݘB	�B	�OB	�B	ݲB	�5B	�5B	��B	��B	��B	��B	�5B	�B	�VB	��B	�OB	�OB	��B	ޞB	�B	�pB	�pB	�;B	�!B	�BB	�'B	�BB	�B	�vB	�vB	�NB	��B	�-B	�B	�tB	�B	�B	�hB	�B	�B	�2B	�B	�fB	�,B	�B	��B	�BB	�vB	�B	��B	��B	�B	��B	�B	��B	�kB	�wB	�5B	�5B	�]B	�B	��B	��B	�6B	� B	�B	�}B	�CB	�)B	�}B	�B	�WB	�=B	�=B	��B	�B	�B	�WB	�qB	�B	�[B	��B	��B	�-B	�3B	�hB	�B	�B	�MB	��B	�B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	�;B	�}B	�/B	�"B	�B	��B	��B	�B	�B	�3B	��B	�B	�B	�B	�[B	�B	�qB	��B	�B	�3B	��B	�`B	��B	�zB	�FB	��B	�B	��B	��B	�B	��B	�$B	��B	��B	�8B	��B	�$B	��B	��B	�dB	�jB	�"B	��B	��B	��B	�(B	��B	��B	�B
 4B	��B	�B	�.B	��B	�wB	�VB	�VB	��B	��B	�qB	�jB	�^B	�B	�B	�B	��B	�<B	��B	�B	��B	��B	��B	��B	�B	�"B	��B	�B	��B	��B	�B	�qB	��B
 �B
B
 �B
 �B
UB
;B
�B
�B
GB
-B
B
B
�B
{B
�B
�B
�B
�B
�B
tB
�B
�B
	RB
	�B

	B
�B
�B
JB
B
�B
B
�B
�B
�B
vB
�B
B
HB
.B
HB
bB
}B
bB
HB
�B
4B
NB
4B
 B
NB
�B
oB
TB
:B
TB
�B
oB
�B
�B
�B
�B
[B
{B
�B
�B
MB
�B
B
�B
�B
�B
�B
�B
�B
_B
+B
yB
yB
�B
1B
KB
�B
B
7B
7B
QB
�B
	B
qB
qB
�B
�B
dB
�B
�B
�B
5B
!B
B
VB
�B
 BB
!HB
!HB
!�B
!�B
!�B
!|B
!�B
!�B
!�B
"hB
"hB
"hB
"hB
"hB
!�B
"�B
"�B
#:B
# B
$�B
%,B
%B
$�B
%B
%,B
'B
'mB
($B
(�B
(�B
(�B
(�B
)yB
)�B
*eB
*�B
*�B
*�B
+kB
+�B
+�B
,"B
,�B
-wB
.B
.cB
.cB
.cB
.�B
.�B
.�B
.�B
/iB
/�B
/�B
/�B
/�B
/�B
/OB
/iB
/�B
0!B
0oB
1B
1B
1AB
1�B
1�B
1�B
1�B
1�B
1�B
2B
2|B
1�B
2aB
2GB
2aB
2GB
2�B
2�B
3B
33B
33B
33B
3MB
3�B
3�B
49B
4TB
4nB
4�B
4�B
4�B
5?B
6+B
6FB
6�B
7LB
7LB
7�B
7�B
7fB
7�B
7�B
7�B
7�B
7�B
8B
8lB
8RB
8�B
9	B
9�B
9�B
9�B
9�B
:*B
:^B
:�B
:�B
:�B
;B
;dB
;B
;�B
;�B
<jB
<�B
<�B
<�B
="B
=<B
=qB
=�B
=�B
>wB
>�B
>�B
?B
?�B
@iB
@iB
@�B
@�B
@�B
AB
AB
A B
AoB
A�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
CB
CaB
C�B
C�B
D3B
D�B
D�B
D�B
EB
E9B
ESB
E�B
E�B
FB
F�B
F�B
G�B
G�B
G�B
G�B
HB
HKB
H�B
HfB
HfB
H�B
IB
I7B
I�B
I�B
JrB
JrB
J�B
J�B
J�B
KB
KDB
KDB
KDB
KxB
K�B
K�B
LdB
L0B
LJB
LdB
L~B
MB
M�B
MjB
MjB
M�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
NB
NB
N<B
N�B
N�B
N�B
O(B
O(B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
PbB
P�B
P�B
P�B
P}B
P}B
P�B
P�B
Q B
QB
QNB
QNB
QhB
QhB
Q�B
Q�B
Q�B
R B
RTB
R�B
R�B
R�B
R�B
S@B
S[B
S�B
S�B
S�B
S�B
TB
T,B
T,B
T,B
TFB
T�B
T�B
T�B
U2B
U�B
U�B
VB
V9B
VmB
V�B
V�B
V�B
WsB
W�B
XB
XEB
XyB
X�B
X�B
YKB
YeB
YeB
YeB
YB
Y�B
Y�B
ZB
ZB
ZQB
ZkB
ZkB
Z�B
[	B
[qB
[�B
[�B
[�B
\B
\B
\]B
\�B
\�B
\�B
\�B
\�B
]~B
^B
]�B
]�B
^B
^B
]�B
^B
]�B
^B
^B
^5B
^5B
^5B
^B
]�B
^B
^B
^B
^B
^5B
^5B
^5B
^5B
^jB
_pB
_B
^�B
_�B
`BB
`BB
`BB
`\B
`�B
`�B
`�B
`�B
a-B
a-B
abB
a�B
b�B
b�B
b�B
b�B
b�B
cB
b�B
c B
c�B
c�B
dtB
d�B
eB
eFB
e�B
e�B
e�B
e�B
e�B
e�B
f2B
ffB
f�B
f�B
gRB
g�B
hXB
h�B
h�B
h�B
h�B
iB
iB
iB
i*B
i*B
i*B
iDB
iyB
iyB
iyB
i�B
i�B
j0B
j�B
j�B
kB
kQB
kkB
lB
lWB
mB
mCB
mwB
m�B
m�B
m�B
m]B
mwB
m�B
ncB
n}B
o B
o B
o5B
o5B
o�B
o�B
o�B
p!B
pUB
p�B
p�B
q'B
q�B
rB
q�B
rGB
r�B
sB
shB
s�B
s�B
s�B
tB
tB
t�B
t�B
uB
u%B
u�B
u�B
u�B
u�B
v11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B��B��B��B�[B�B�AB�uB��B�-B�B�aB�-B�GB�B�aB�{B��B��B��By>BT�BTFBW�B[qB\]B]B`'Ba�BbhBe�BvzBx�B�B��B��B�B��Bm�Bm�B.B�QB��B�B	=�B	kQB	�B	}�B	��B	خB	��B	��B
�B
!�B
 B
.�B
?.B
j�B
��B
��B
�B
�}B
�dB
��B
�GB
�B
�=B
P}B
B
�B
!bB
8B
U�B
l�B
|jB
�B
�B
��B
LdB
@OB
9�B
8�B
NVB
��B[B�B
�!B
��B
��B
�B
�B
��B
jB
KB
�B	��B	��B	�B	��B	bNB	MB	"�B		�B��B�B�/B��B�}B�jB��B�#B�MB�bB��B�ZB	�B	vB	�B	bB	:B	NB	�B	�B	DB	{B	�B	-)B	7fB	N�B	jB	kB	nB	n�B	rGB	x�B	��B	�oB	�B	|�B	�@B	��B	�PB	�&B	�uB	ԯB	�9B	�OB	�B	�
B	�B	�B	�B	�@B	�B	�B	�EB	�aB	�,B	�7B	��B	�B	��B	�B	��B	��B	�B	�=B	�IB	��B	�dB	�{B	��B	�B	چB	��B	�$B	�B	��B	�KB	��B	��B	ּB	��B	��B	��B	�+B	׍B	��B	خB	�OB	�4B	��B	޸B	��B	�IB	�B	�xB	ܬB	�IB	��B	�jB	ޞB	�;B	޸B	�!B	�B	�jB	�jB	޸B	�B	ޞB	ޞB	ޞB	ބB	ބB	�jB	�B	�OB	�B	��B	�5B	ޞB	�5B	�jB	��B	�B	ބB	�!B	��B	�]B	��B	�#B	�WB	�	B	��B	��B	�$B	�mB	�B	��B	�{B	�,B	�B	�[B	҉B	�:B	��B	��B	�B	�[B	ӏB	ӏB	��B	ԕB	յB	�+B	�1B	�B	�yB	�YB	��B	�eB	ٴB	�kB	ٴB	�kB	چB	ںB	��B	چB	��B	��B	ܒB	ݘB	�B	�OB	�B	ݲB	�5B	�5B	��B	��B	��B	��B	�5B	�B	�VB	��B	�OB	�OB	��B	ޞB	�B	�pB	�pB	�;B	�!B	�BB	�'B	�BB	�B	�vB	�vB	�NB	��B	�-B	�B	�tB	�B	�B	�hB	�B	�B	�2B	�B	�fB	�,B	�B	��B	�BB	�vB	�B	��B	��B	�B	��B	�B	��B	�kB	�wB	�5B	�5B	�]B	�B	��B	��B	�6B	� B	�B	�}B	�CB	�)B	�}B	�B	�WB	�=B	�=B	��B	�B	�B	�WB	�qB	�B	�[B	��B	��B	�-B	�3B	�hB	�B	�B	�MB	��B	�B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	�;B	�}B	�/B	�"B	�B	��B	��B	�B	�B	�3B	��B	�B	�B	�B	�[B	�B	�qB	��B	�B	�3B	��B	�`B	��B	�zB	�FB	��B	�B	��B	��B	�B	��B	�$B	��B	��B	�8B	��B	�$B	��B	��B	�dB	�jB	�"B	��B	��B	��B	�(B	��B	��B	�B
 4B	��B	�B	�.B	��B	�wB	�VB	�VB	��B	��B	�qB	�jB	�^B	�B	�B	�B	��B	�<B	��B	�B	��B	��B	��B	��B	�B	�"B	��B	�B	��B	��B	�B	�qB	��B
 �B
B
 �B
 �B
UB
;B
�B
�B
GB
-B
B
B
�B
{B
�B
�B
�B
�B
�B
tB
�B
�B
	RB
	�B

	B
�B
�B
JB
B
�B
B
�B
�B
�B
vB
�B
B
HB
.B
HB
bB
}B
bB
HB
�B
4B
NB
4B
 B
NB
�B
oB
TB
:B
TB
�B
oB
�B
�B
�B
�B
[B
{B
�B
�B
MB
�B
B
�B
�B
�B
�B
�B
�B
_B
+B
yB
yB
�B
1B
KB
�B
B
7B
7B
QB
�B
	B
qB
qB
�B
�B
dB
�B
�B
�B
5B
!B
B
VB
�B
 BB
!HB
!HB
!�B
!�B
!�B
!|B
!�B
!�B
!�B
"hB
"hB
"hB
"hB
"hB
!�B
"�B
"�B
#:B
# B
$�B
%,B
%B
$�B
%B
%,B
'B
'mB
($B
(�B
(�B
(�B
(�B
)yB
)�B
*eB
*�B
*�B
*�B
+kB
+�B
+�B
,"B
,�B
-wB
.B
.cB
.cB
.cB
.�B
.�B
.�B
.�B
/iB
/�B
/�B
/�B
/�B
/�B
/OB
/iB
/�B
0!B
0oB
1B
1B
1AB
1�B
1�B
1�B
1�B
1�B
1�B
2B
2|B
1�B
2aB
2GB
2aB
2GB
2�B
2�B
3B
33B
33B
33B
3MB
3�B
3�B
49B
4TB
4nB
4�B
4�B
4�B
5?B
6+B
6FB
6�B
7LB
7LB
7�B
7�B
7fB
7�B
7�B
7�B
7�B
7�B
8B
8lB
8RB
8�B
9	B
9�B
9�B
9�B
9�B
:*B
:^B
:�B
:�B
:�B
;B
;dB
;B
;�B
;�B
<jB
<�B
<�B
<�B
="B
=<B
=qB
=�B
=�B
>wB
>�B
>�B
?B
?�B
@iB
@iB
@�B
@�B
@�B
AB
AB
A B
AoB
A�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
CB
CaB
C�B
C�B
D3B
D�B
D�B
D�B
EB
E9B
ESB
E�B
E�B
FB
F�B
F�B
G�B
G�B
G�B
G�B
HB
HKB
H�B
HfB
HfB
H�B
IB
I7B
I�B
I�B
JrB
JrB
J�B
J�B
J�B
KB
KDB
KDB
KDB
KxB
K�B
K�B
LdB
L0B
LJB
LdB
L~B
MB
M�B
MjB
MjB
M�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
NB
NB
N<B
N�B
N�B
N�B
O(B
O(B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
PbB
P�B
P�B
P�B
P}B
P}B
P�B
P�B
Q B
QB
QNB
QNB
QhB
QhB
Q�B
Q�B
Q�B
R B
RTB
R�B
R�B
R�B
R�B
S@B
S[B
S�B
S�B
S�B
S�B
TB
T,B
T,B
T,B
TFB
T�B
T�B
T�B
U2B
U�B
U�B
VB
V9B
VmB
V�B
V�B
V�B
WsB
W�B
XB
XEB
XyB
X�B
X�B
YKB
YeB
YeB
YeB
YB
Y�B
Y�B
ZB
ZB
ZQB
ZkB
ZkB
Z�B
[	B
[qB
[�B
[�B
[�B
\B
\B
\]B
\�B
\�B
\�B
\�B
\�B
]~B
^B
]�B
]�B
^B
^B
]�B
^B
]�B
^B
^B
^5B
^5B
^5B
^B
]�B
^B
^B
^B
^B
^5B
^5B
^5B
^5B
^jB
_pB
_B
^�B
_�B
`BB
`BB
`BB
`\B
`�B
`�B
`�B
`�B
a-B
a-B
abB
a�B
b�B
b�B
b�B
b�B
b�B
cB
b�B
c B
c�B
c�B
dtB
d�B
eB
eFB
e�B
e�B
e�B
e�B
e�B
e�B
f2B
ffB
f�B
f�B
gRB
g�B
hXB
h�B
h�B
h�B
h�B
iB
iB
iB
i*B
i*B
i*B
iDB
iyB
iyB
iyB
i�B
i�B
j0B
j�B
j�B
kB
kQB
kkB
lB
lWB
mB
mCB
mwB
m�B
m�B
m�B
m]B
mwB
m�B
ncB
n}B
o B
o B
o5B
o5B
o�B
o�B
o�B
p!B
pUB
p�B
p�B
q'B
q�B
rB
q�B
rGB
r�B
sB
shB
s�B
s�B
s�B
tB
tB
t�B
t�B
uB
u%B
u�B
u�B
u�B
u�B
v11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104953  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604175235  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604175235  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604175235                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605025243  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605025243  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610141505                      G�O�G�O�G�O�                