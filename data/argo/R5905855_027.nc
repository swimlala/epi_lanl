CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T19:15:28Z creation;2022-06-04T19:15:28Z conversion to V3.1      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604191528  20220610151508  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @��E[�S1   @��E�Es@0�Ƨ�d�G�{1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@��BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�ffB�  B�ffB���B�  B�  B�33B�ffB�33B���B�  B�  B�  B�  B�  B�ffB�  B�  B�33B���B�  B�  C   C  C  C  C  C
  C  C  C  C  C�C  C  C  C  C  C   C!�fC$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@�CB33CC�fCE�fCH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf33Cg�fCj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @=q@�Q�@�Q�A (�A (�A@(�A`(�A�{A�{A�{A�{A�{A�{A�{A�{B 
=B
=B
=B
=B 
=B(
=B0
=B8
=B@�
BH
=BP
=BX
=B`
=Bh
=Bp
=Bx
=B�B�B�B�B�B�B�B���B�B�B�k�B�B�k�B���B�B�B�8RB�k�B�8RB���B�B�B�B�B�B�k�B�B�B�8RB���B�B�C �C�C�C�C�C
�C�C�C�C�C)C�C�C�C�C�C �C!��C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@)CB5�CC��CE��CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf5�Cg��Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC��{C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HD  �D ��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D	 �D	��D
 �D
��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D  �D ��D! �D!��D" �D"��D# �D#��D$ �D$��D% �D%��D& �D&��D' �D'��D( �D(��D) �D)��D* �D*��D+ �D+��D, �D,��D- �D-��D. �D.��D/ �D/��D0 �D0��D1 �D1��D2 �D2��D3 �D3��D4 �D4��D5 �D5��D6 �D6��D7 �D7��D8 �D8��D9 �D9��D: �D:��D; �D;��D< �D<��D= �D=��D> �D>��D? �D?��D@ �D@��DA �DA��DB �DB��DC �DC��DD �DD��DE �DE��DF �DF��DG �DG��DH �DH��DI �DI��DJ �DJ��DK �DK��DL �DL��DM �DM��DN �DN��DO �DO��DP �DP��DQ �DQ��DR �DR��DS �DS��DT �DT��DU �DU��DV �DV��DW �DW��DX �DX��DY �DY��DZ �DZ��D[ �D[��D\ �D\��D] �D]��D^ �D^��D_ �D_��D` �D`��Da �Da��Db �Db��Dc �Dc��Dd �Dd��De �De��Df �Df��Dg �Dg��Dh �Dh��Di �Di��Dj �Dj��Dk �Dk��Dl �Dl��Dm �Dm��Dn �Dn��Do �Do��Dp �Dp��Dq �Dq��Dr �Dr��Ds �Ds��Dt �Dt��Du �Du��Dv �Dv��Dw �Dw��Dx �Dx��Dy �Dy��Dz �Dz��D{ �D{��D| �D|��D} �D}��D~ �D~��D �D��D� RD�C�D��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RDRD��RD� RD�@RDÀRD��RD� RD�@RDĀRD��RD� RD�@RDŀRD��RD� RD�@RDƀRD��RD� RD�@RDǀRD��RD� RD�@RDȀRD��RD� RD�@RDɀRD��RD� RD�@RDʀRD��RD� RD�@RDˀRD��RD� RD�@RD̀RD��RD� RD�@RD̀RD��RD� RD�@RD΀RD��RD� RD�@RDπRD��RD� RD�@RDЀRD��RD� RD�@RDрRD��RD� RD�@RDҀRD��RD� RD�@RDӀRD��RD� RD�@RDԀRD��RD� RD�@RDՀRD��RD� RD�@RDրRD��RD� RD�@RD׀RD��RD� RD�@RD؀RD��RD� RD�@RDـRD��RD� RD�@RDڀRD��RD� RD�@RDۀRD��RD� RD�@RD܀RD��RD� RD�@RD݀RD��RD� RD�@RDހRD��RD� RD�@RD߀RD��RD� RD�@RD��RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD��RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�]�A�F?A�L�A�2aA��A�YA��fA���A��,A��vA���A��BA��KA�� A߾A߼AߨXA�R A��A��A�SA��A��A��A� �A��A��HAޱ�AނuA��Aݼ6A�p�A�6zA��A��AܨXA�k�A��KA�zxA�5?A�:�A��oAӏ(Aҕ�A�רA̟�A�=�A��/A�g�A�+�A��kA���A��?A�2�A�pA�&�A�VA��A�U�A��bA�S&A�e�A�ʌA��9A��lA��A���A���A�u�A�k�A�ΥA�%A���A�x�A�g8A�Z�A���A���A��	A��A���A��A�YA���A��XA�z�A�F?A��A��QA��]A�F�A�?A���A� �A�ȀA�C-A�x�A���A���A{h�Aw�Ar�XAn�QAjAeC�A`�AA\_�ASU�ARO�APT�AL��AJX�AG��AF�AD��ACGEAA-wA?j�A<ĜA:��A8�A7bA6VmA5��A4�A3(�A1��A1FA0�YA0
=A/}�A.�A.P�A-5�A,ffA+�[A+oiA+�A)�:A(�A'��A&��A%�LA%U�A%YA$��A$	lA"�HA"M�A!��A	lAH�A��A��A�AFAX�A�1A �A�	A�"A� A�!A�[A��Av`A \A��AߤA��AP�A�IA�wA�FA}�A�OAy�A�fA��A_AMjA��A�A�jA�A��A��AQ�A��A�AA�A�A�}A�;A�A�jAMA�A=�A/�A�A��A�A��A?�A��A��AbNAi�A&�A
�'A
S�A	��A	8A|AK^A �A�A2�AoA��A/AݘA��A6A��A6zA�MA�A��AffAc AJ#A($A�A�CA�"AkQADgA!A��A~�Ay>Ae,AeA cA �4@���@�!�@��B@��z@�9X@��@���@��Y@��@�˒@�33@�ff@�/�@���@�@�%�@�q@�J#@��v@�r�@��T@�҉@�($@�c�@�*0@���@��v@�ѷ@�>B@�^�@점@�i�@�@�|�@ꖼ@�@�e,@�&@��@���@�F@�IR@���@歬@�@唯@��@�ȴ@��'@�4@�x�@��P@⭬@� @�^5@�$�@�<6@���@��Z@�4@���@ޡb@�($@�o @�}V@��@�|�@��8@��@�w2@��@��m@��@ט�@׏�@�F�@��@�{�@�
�@�ѷ@��g@�%@үO@�n�@�x@�ݘ@Ѯ@�S&@Ю}@�|�@�Q�@�@ϧ�@�=@��"@Μx@�I�@��@��"@�($@���@�@�A�@�h
@���@���@�Mj@���@Ƞ�@�m�@��@�?}@� i@�9�@� \@�ȴ@�z�@�I�@�)�@�b@Ő�@��@��@��@��
@à'@�<6@��@��F@��z@�خ@�H�@��$@�Z�@�Ta@�7�@��@��P@�j�@�rG@�c�@�_p@��@��@�YK@�!@���@��#@���@�e�@�#�@��@�ߤ@��L@�U2@�U�@���@�$�@��D@���@�>�@��	@��[@���@��O@���@�<�@���@��g@�0�@�{@���@�$t@���@��/@�ں@��@��E@���@�1@��-@�o @�G�@���@�K^@�O@���@��@��X@��@��f@��]@��8@��@��@� �@��@��Z@���@��@�H�@��o@��H@��@�c�@�$t@��]@�z�@��@�1@���@���@�J#@��5@���@�n�@�-�@��@��
@���@�J�@��F@��@���@���@�J�@�g8@���@�b�@�=�@���@�L0@���@���@�iD@�0�@��@��B@�l�@���@�/�@�p;@�1@�|@��@��U@��z@�|�@�S�@��@��^@�j�@�/�@�@���@���@���@�_@�H@�@��&@��k@��@�x�@�a@��@���@���@���@�E9@���@��6@�v�@�$@��D@���@��o@���@���@��@��@��Y@��@���@��@��@��@�o @�L�@�@O@�4@�/�@��@��@��@�v�@��@���@��~@�n/@�0�@��@��H@���@�	�@��D@��A@�w2@�%@��L@�`�@��@��d@��3@���@���@�O�@�*0@�q@���@��E@���@���@�:�@���@��V@���@���@�w2@�_p@�@O@��@���@���@��<@��}@��b@��Y@�xl@�0U@��@���@�}�@�\)@�?}@��@���@�҉@��j@���@���@�v�@�i�@�Z�@�?�@�_@���@�ԕ@��-@�~�@�^�@�C@���@���@���@�ff@�5?@� �@��o@���@���@��@�c@�iD@�8�@�(@���@�֡@��@��@_p@~}V@}��@}@|�5@|_@{�;@{��@{�@z�R@za|@zE�@ze@y�t@y^�@x�?@xl"@xb@w��@w'�@v��@v��@v($@u��@tی@th�@tH@s�W@r�@q�=@p��@p�@pg8@p%�@o��@o�@nz@m�@l]d@k�]@k�P@k;d@kY@j��@k$t@j��@jV@i�)@i��@iN<@i	l@h��@h4n@g�]@g��@giD@g"�@f��@fff@e��@e|@e@@d��@d�p@d�z@d>B@c�[@b��@b�+@b�@a�@a�'@`�P@`�@`4n@`	�@`'R@`/�@`6@`4n@`/�@`M@_v`@^��@^h
@]��@]%@\�U@\��@\7@[��@[\)@Z�y@Z#:@YF@X�@XI�@Xb@W��@W�P@Wx@WMj@W�@V��@VR�@U��@U|@Uc�@T�$@TQ�@TA�@T7�@S�@St�@SJ#@S.I@R�c@R_�@Q@Pw�@O�]@O˒@O��@OP�@O@N��@N��@M�^@MY�@L�`@L�@L�$@Le�@LI�@L�@K�0@K��@K��@Kn/@K\)@K;d@KS@J�H@J��@J{�@J#:@I��@IL�@H��@H��@G˒@G��@Ga@G=@Go@F��@F �@E�'@E�@ES&@EDg@D��@D�@Dz�@D  @C��@C�@@CX�@B�M@B��@Bv�@BW�@B�@A�j@ArG@@��@@��@@tT@@V�@@!@?dZ@?H�@?J#@?33@?�@>�2@>��@>�r@>n�@>;�@>�@=�T@=�N@=��@=�@=�@==�@<�@<�I@;�
@;A�@;@:�]@:��@:W�@:L0@::*@:
�@9��@9e,@8��@8�v@8�@8��@8��@86@8@7�}@7�V@7a@6�@6��@6xl@6Q@6?@6$�@5��@5�h@5�@4��@4�)@4�4@4u�@4M@4:�@3��@3خ@3��@3l�@3U�@3�@2xl@2�@2	@1��@1J�@0��@0PH@0x@/��@.�@.�r@.&�@-��@-#�@,ѷ@,��@,�e@,��@,?�@,x@+��@+o�@+O@*�,@*:*@*J@)�@)�@)��@)�9@)��@)�@)��@)��@)��@)`B@(�@(~(@(�@'�F@'x@'Z�@'J#@'6z@'$t@'�@&��@&p;@&E�@&e@%�>@%��@%m]@%7L@%�@$�v@$�D@$PH@$C-@$1'@$x@#˒@#��@#{J@#&@"͟@"Z�@" �@!�@!�@!f�@!?}@!(�@!%@ ֡@ �O@ �I@ _@ 4n@ M@�Q@�@@��@dZ@!-@�8@��@��@�@m]@�|@��@c�@V�@,=@��@��@�:@�4@iD@A�@!-@�B@c @�@�@�)@��@4n@�@�0@n/@W?@@O@,�@@�B@_�@C�@{@�@�N@��@u�@Dg@q@�5@�O@�I@��@�@w�@j@_@V�@Ft@@�Q@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�]�A�F?A�L�A�2aA��A�YA��fA���A��,A��vA���A��BA��KA�� A߾A߼AߨXA�R A��A��A�SA��A��A��A� �A��A��HAޱ�AނuA��Aݼ6A�p�A�6zA��A��AܨXA�k�A��KA�zxA�5?A�:�A��oAӏ(Aҕ�A�רA̟�A�=�A��/A�g�A�+�A��kA���A��?A�2�A�pA�&�A�VA��A�U�A��bA�S&A�e�A�ʌA��9A��lA��A���A���A�u�A�k�A�ΥA�%A���A�x�A�g8A�Z�A���A���A��	A��A���A��A�YA���A��XA�z�A�F?A��A��QA��]A�F�A�?A���A� �A�ȀA�C-A�x�A���A���A{h�Aw�Ar�XAn�QAjAeC�A`�AA\_�ASU�ARO�APT�AL��AJX�AG��AF�AD��ACGEAA-wA?j�A<ĜA:��A8�A7bA6VmA5��A4�A3(�A1��A1FA0�YA0
=A/}�A.�A.P�A-5�A,ffA+�[A+oiA+�A)�:A(�A'��A&��A%�LA%U�A%YA$��A$	lA"�HA"M�A!��A	lAH�A��A��A�AFAX�A�1A �A�	A�"A� A�!A�[A��Av`A \A��AߤA��AP�A�IA�wA�FA}�A�OAy�A�fA��A_AMjA��A�A�jA�A��A��AQ�A��A�AA�A�A�}A�;A�A�jAMA�A=�A/�A�A��A�A��A?�A��A��AbNAi�A&�A
�'A
S�A	��A	8A|AK^A �A�A2�AoA��A/AݘA��A6A��A6zA�MA�A��AffAc AJ#A($A�A�CA�"AkQADgA!A��A~�Ay>Ae,AeA cA �4@���@�!�@��B@��z@�9X@��@���@��Y@��@�˒@�33@�ff@�/�@���@�@�%�@�q@�J#@��v@�r�@��T@�҉@�($@�c�@�*0@���@��v@�ѷ@�>B@�^�@점@�i�@�@�|�@ꖼ@�@�e,@�&@��@���@�F@�IR@���@歬@�@唯@��@�ȴ@��'@�4@�x�@��P@⭬@� @�^5@�$�@�<6@���@��Z@�4@���@ޡb@�($@�o @�}V@��@�|�@��8@��@�w2@��@��m@��@ט�@׏�@�F�@��@�{�@�
�@�ѷ@��g@�%@үO@�n�@�x@�ݘ@Ѯ@�S&@Ю}@�|�@�Q�@�@ϧ�@�=@��"@Μx@�I�@��@��"@�($@���@�@�A�@�h
@���@���@�Mj@���@Ƞ�@�m�@��@�?}@� i@�9�@� \@�ȴ@�z�@�I�@�)�@�b@Ő�@��@��@��@��
@à'@�<6@��@��F@��z@�خ@�H�@��$@�Z�@�Ta@�7�@��@��P@�j�@�rG@�c�@�_p@��@��@�YK@�!@���@��#@���@�e�@�#�@��@�ߤ@��L@�U2@�U�@���@�$�@��D@���@�>�@��	@��[@���@��O@���@�<�@���@��g@�0�@�{@���@�$t@���@��/@�ں@��@��E@���@�1@��-@�o @�G�@���@�K^@�O@���@��@��X@��@��f@��]@��8@��@��@� �@��@��Z@���@��@�H�@��o@��H@��@�c�@�$t@��]@�z�@��@�1@���@���@�J#@��5@���@�n�@�-�@��@��
@���@�J�@��F@��@���@���@�J�@�g8@���@�b�@�=�@���@�L0@���@���@�iD@�0�@��@��B@�l�@���@�/�@�p;@�1@�|@��@��U@��z@�|�@�S�@��@��^@�j�@�/�@�@���@���@���@�_@�H@�@��&@��k@��@�x�@�a@��@���@���@���@�E9@���@��6@�v�@�$@��D@���@��o@���@���@��@��@��Y@��@���@��@��@��@�o @�L�@�@O@�4@�/�@��@��@��@�v�@��@���@��~@�n/@�0�@��@��H@���@�	�@��D@��A@�w2@�%@��L@�`�@��@��d@��3@���@���@�O�@�*0@�q@���@��E@���@���@�:�@���@��V@���@���@�w2@�_p@�@O@��@���@���@��<@��}@��b@��Y@�xl@�0U@��@���@�}�@�\)@�?}@��@���@�҉@��j@���@���@�v�@�i�@�Z�@�?�@�_@���@�ԕ@��-@�~�@�^�@�C@���@���@���@�ff@�5?@� �@��o@���@���@��@�c@�iD@�8�@�(@���@�֡@��@��@_p@~}V@}��@}@|�5@|_@{�;@{��@{�@z�R@za|@zE�@ze@y�t@y^�@x�?@xl"@xb@w��@w'�@v��@v��@v($@u��@tی@th�@tH@s�W@r�@q�=@p��@p�@pg8@p%�@o��@o�@nz@m�@l]d@k�]@k�P@k;d@kY@j��@k$t@j��@jV@i�)@i��@iN<@i	l@h��@h4n@g�]@g��@giD@g"�@f��@fff@e��@e|@e@@d��@d�p@d�z@d>B@c�[@b��@b�+@b�@a�@a�'@`�P@`�@`4n@`	�@`'R@`/�@`6@`4n@`/�@`M@_v`@^��@^h
@]��@]%@\�U@\��@\7@[��@[\)@Z�y@Z#:@YF@X�@XI�@Xb@W��@W�P@Wx@WMj@W�@V��@VR�@U��@U|@Uc�@T�$@TQ�@TA�@T7�@S�@St�@SJ#@S.I@R�c@R_�@Q@Pw�@O�]@O˒@O��@OP�@O@N��@N��@M�^@MY�@L�`@L�@L�$@Le�@LI�@L�@K�0@K��@K��@Kn/@K\)@K;d@KS@J�H@J��@J{�@J#:@I��@IL�@H��@H��@G˒@G��@Ga@G=@Go@F��@F �@E�'@E�@ES&@EDg@D��@D�@Dz�@D  @C��@C�@@CX�@B�M@B��@Bv�@BW�@B�@A�j@ArG@@��@@��@@tT@@V�@@!@?dZ@?H�@?J#@?33@?�@>�2@>��@>�r@>n�@>;�@>�@=�T@=�N@=��@=�@=�@==�@<�@<�I@;�
@;A�@;@:�]@:��@:W�@:L0@::*@:
�@9��@9e,@8��@8�v@8�@8��@8��@86@8@7�}@7�V@7a@6�@6��@6xl@6Q@6?@6$�@5��@5�h@5�@4��@4�)@4�4@4u�@4M@4:�@3��@3خ@3��@3l�@3U�@3�@2xl@2�@2	@1��@1J�@0��@0PH@0x@/��@.�@.�r@.&�@-��@-#�@,ѷ@,��@,�e@,��@,?�@,x@+��@+o�@+O@*�,@*:*@*J@)�@)�@)��@)�9@)��@)�@)��@)��@)��@)`B@(�@(~(@(�@'�F@'x@'Z�@'J#@'6z@'$t@'�@&��@&p;@&E�@&e@%�>@%��@%m]@%7L@%�@$�v@$�D@$PH@$C-@$1'@$x@#˒@#��@#{J@#&@"͟@"Z�@" �@!�@!�@!f�@!?}@!(�@!%@ ֡@ �O@ �I@ _@ 4n@ M@�Q@�@@��@dZ@!-@�8@��@��@�@m]@�|@��@c�@V�@,=@��@��@�:@�4@iD@A�@!-@�B@c @�@�@�)@��@4n@�@�0@n/@W?@@O@,�@@�B@_�@C�@{@�@�N@��@u�@Dg@q@�5@�O@�I@��@�@w�@j@_@V�@Ft@@�Q@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B	��B	��B	�B	�B	��B	�KB	�B	�yB	�B	��B	�B	�*B	�B	�
B	��B	�B	�yB	�yB	�B	�ZB	��B	�2B	�mB	��B	��B	�WB	��B
 �B
�B
�B
�B
�B
�B
�B
KB
�B
 B
�B
yB
&�B
)�B
>�B
;�B
0!B
8RB
A�B
:�B
�B
��B
��B
�-B
��B
҉B
��B�B,�BM�BW�Bf2BpUB�B��B��B��B�)B��B��B�9B��B�tB�lB�B�HB�%B�GB�cB��B��B�DB�Br�BXyBKB=VB.�B-wB!�BB�B
�FB
��B
��B
��B
��B
�B
iyB
KxB
#TB	��B	ΊB	��B	��B	�iB	^jB	ESB	0�B	=B��B�B��B�B��B�PB�^B҉BބB�mB�tB��B��B	DB	&B	�B	mB	EB	�B	"�B	#nB	)�B	-]B	3MB	9XB	CB	X�B	gRB	t�B	�4B	��B	��B	� B	��B	��B	�HB	�RB	�XB	��B	��B	�B	�B	��B	͟B	��B	ÖB	̳B	��B	߾B	�B	��B	��B	�B	��B
-B
�B
�B
�B
bB
�B
:B
sB
�B
&B
fB
 B
�B
 �B
�B
B	��B	ңB	�?B	��B	�B	�HB	��B	� B	ՁB	��B	�B	�B	��B
gB
B
zB
1B
	B
DB
B
�B
1B

�B
	�B
	�B
B
BB
�B
VB
�B
�B
�B
�B
�B
FB
oB
B
B
�B
�B
+B
B
�B
gB
{B
B
�B
�B
B
�B
HB
�B
�B
B
�B
TB
�B
 B
B
�B
(B
VB
�B
bB
vB
vB
�B
�B
B
B
�B
�B
�B
B
�B
�B
�B
�B
�B
JB
dB
�B
^B

=B

�B
)B

�B
	lB
�B
�B
KB
�B
�B
B
3B
�B
�B
�B
�B
�B
  B
 �B
oB
 �B	�wB	��B	��B
 B
B
 B	�.B	��B	��B	�]B	�]B	�B	��B	��B	��B
�B
aB
B
B
�B
�B
aB
'B
 �B
 �B
 OB
 B	�HB	�]B	��B	��B	��B	��B	��B	�^B	�XB	�$B	��B	�xB	�dB	��B	�B	��B	�B	�	B	��B	�2B	��B	�2B	�2B	��B	�LB	�LB	��B	��B	�B	�B	�B	��B	�dB	�0B	�0B	��B	��B	��B	��B	�JB	�B	�B	�JB	�0B	��B	�JB	��B	��B	�dB	�JB	�B	��B	�.B
  B
 �B
 B	�HB	��B	�]B	�BB	��B	�cB	�B	��B	��B	��B	�B	�B	�^B	�rB	�rB	��B	��B	��B	�*B	��B	�B	�0B	�B	��B	��B	�B	��B	��B	��B	�jB	��B	��B	��B	��B	�B	��B	�BB	�(B
  B
 iB
B
B
oB
�B
 �B
B
B
 �B
 �B
 �B
 �B
  B	�]B	��B	��B	��B	�<B	��B	��B	��B	��B	��B	�.B	�HB	�cB	�cB
 �B
 �B
 �B
 �B
 �B
 �B
 �B
 B
  B	��B	��B	�wB	�wB	�(B	��B	�BB	��B
 OB
UB
�B
B
'B
�B
�B
�B
�B
SB
�B
EB
�B
	lB
	�B
	�B

�B

�B

�B
)B

�B
B
	�B
	�B
	B
�B
�B
�B
�B
�B
�B
zB
�B
�B
�B
zB
�B
KB
�B
�B
	�B

�B

�B
�B
JB
�B
�B
�B
�B
dB
PB
jB
�B
�B
"B
<B
�B
�B
�B
BB
vB
�B
�B
�B
�B
HB
�B
�B
bB
�B
�B
�B
�B
B
�B
.B
�B
�B
�B
�B
}B
�B
NB
�B
�B
�B
&B
�B
�B
�B
2B
gB
B
sB
B
�B
�B
�B
�B
�B
	B
�B
�B
B
�B
xB
CB
B
~B
�B
OB
�B
�B
 BB
 \B
!bB
!�B
!�B
!�B
"B
"B
"4B
"4B
"�B
#:B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
#B
#B
#B
#B
#TB
$B
$&B
$@B
$�B
$�B
%B
%,B
%zB
%�B
%�B
%�B
%�B
%�B
%�B
&�B
'mB
'�B
'�B
'�B
'�B
(
B
(�B
(�B
)�B
)�B
)�B
*0B
*eB
*eB
*�B
+B
+B
+QB
+kB
+�B
+�B
+�B
,B
,"B
,"B
+�B
,�B
-)B
-�B
./B
./B
-�B
.B
.IB
.�B
.�B
.�B
.�B
/5B
/OB
/�B
0!B
0UB
0�B
1[B
2aB
3MB
4�B
3�B
33B
4B
4�B
5%B
5?B
5ZB
5?B
5%B
5%B
5�B
6+B
6+B
5�B
5�B
5B
4�B
5?B
5%B
5�B
5�B
6�B
7B
7B
7LB
7�B
7�B
7�B
8lB
8�B
8lB
8B
7�B
7�B
7LB
6�B
6�B
6�B
7�B
7�B
88B
8�B
8�B
9XB
9rB
9�B
9�B
:B
:xB
:�B
:�B
;0B
;�B
<�B
=VB
=�B
>wB
?B
?}B
@OB
@iB
@�B
@�B
@�B
AUB
A�B
BB
BAB
BAB
BB
B�B
B�B
B�B
C{B
C�B
C�B
D�B
DgB
D�B
D�B
D�B
D�B
E9B
EmB
EmB
E�B
FYB
FtB
F�B
F�B
G_B
GEB
G+B
GEB
G_B
F�B
G_B
H1B
H�B
H�B
IB
I7B
IRB
I7B
J	B
J�B
KB
K)B
KxB
K^B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
LB
L0B
LdB
L�B
MB
M6B
L�B
L�B
MB
L�B
MB
M�B
M�B
M�B
M�B
NpB
N�B
N�B
N�B
OB
N�B
OBB
OB
OB
O(B
OvB
O�B
O�B
OvB
O\B
OBB
OBB
O�B
P.B
P�B
Q�B
Q�B
QhB
QNB
QNB
Q�B
Q�B
Q�B
RB
RTB
RTB
RoB
R�B
R�B
R�B
R�B
R�B
R�B
R�B
T,B
U�B
U�B
VB
VSB
W
B
W�B
W�B
W�B
XB
X_B
X_B
X_B
X_B
X_B
X_B
W�B
W�B
XyB
X�B
X�B
X�B
X�B
X�B
Y1B
YKB
Y�B
Y�B
Z�B
Z�B
Z�B
Z�B
[#B
[=B
[�B
[WB
[qB
[�B
[�B
\B
\CB
\xB
\]B
\�B
\�B
\�B
]/B
]�B
]�B
]�B
]~B
^5B
^5B
^B
^B
]�B
]�B
]�B
]~B
]/B
]B
\�B
\�B
\�B
\�B
]/B
]B
]�B
]�B
]�B
^jB
_B
_!B
_pB
_pB
_pB
_pB
_pB
_pB
_�B
_�B
_�B
_�B
`'B
`vB
`�B
aHB
a|B
a�B
a|B
a�B
a�B
a�B
a�B
b4B
bhB
b�B
cB
c:B
cTB
cnB
c�B
c�B
c�B
c�B
dB
d&B
dZB
d�B
d�B
d�B
e,B
ezB
e�B
fLB
f�B
f�B
gB
g8B
gRB
gRB
g�B
g�B
g�B
h>B
hsB
h�B
h�B
h�B
iB
i*B
i*B
iDB
iyB
i�B
jeB
j�B
k6B
kkB
k�B
k�B
k�B
l"B
l�B
l�B
l�B
l�B
mB
mB
mwB
m�B
m�B
m�B
m�B
m�B
n/B
nIB
ncB
n�B
n�B
n�B
n�B
n�B
o5B
o�B
o�B
o�B
p!B
p;B
poB
p�B
p�B
p�B
p�B
q[B
qAB
q[B
q[B
qvB
q�B
q�B
q�B
q�B
q�B
q�B
r-11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B	��B	��B	�B	�B	��B	�KB	�B	�yB	�B	��B	�B	�*B	�B	�
B	��B	�B	�yB	�yB	�B	�ZB	��B	�2B	�mB	��B	��B	�WB	��B
 �B
�B
�B
�B
�B
�B
�B
KB
�B
 B
�B
yB
&�B
)�B
>�B
;�B
0!B
8RB
A�B
:�B
�B
��B
��B
�-B
��B
҉B
��B�B,�BM�BW�Bf2BpUB�B��B��B��B�)B��B��B�9B��B�tB�lB�B�HB�%B�GB�cB��B��B�DB�Br�BXyBKB=VB.�B-wB!�BB�B
�FB
��B
��B
��B
��B
�B
iyB
KxB
#TB	��B	ΊB	��B	��B	�iB	^jB	ESB	0�B	=B��B�B��B�B��B�PB�^B҉BބB�mB�tB��B��B	DB	&B	�B	mB	EB	�B	"�B	#nB	)�B	-]B	3MB	9XB	CB	X�B	gRB	t�B	�4B	��B	��B	� B	��B	��B	�HB	�RB	�XB	��B	��B	�B	�B	��B	͟B	��B	ÖB	̳B	��B	߾B	�B	��B	��B	�B	��B
-B
�B
�B
�B
bB
�B
:B
sB
�B
&B
fB
 B
�B
 �B
�B
B	��B	ңB	�?B	��B	�B	�HB	��B	� B	ՁB	��B	�B	�B	��B
gB
B
zB
1B
	B
DB
B
�B
1B

�B
	�B
	�B
B
BB
�B
VB
�B
�B
�B
�B
�B
FB
oB
B
B
�B
�B
+B
B
�B
gB
{B
B
�B
�B
B
�B
HB
�B
�B
B
�B
TB
�B
 B
B
�B
(B
VB
�B
bB
vB
vB
�B
�B
B
B
�B
�B
�B
B
�B
�B
�B
�B
�B
JB
dB
�B
^B

=B

�B
)B

�B
	lB
�B
�B
KB
�B
�B
B
3B
�B
�B
�B
�B
�B
  B
 �B
oB
 �B	�wB	��B	��B
 B
B
 B	�.B	��B	��B	�]B	�]B	�B	��B	��B	��B
�B
aB
B
B
�B
�B
aB
'B
 �B
 �B
 OB
 B	�HB	�]B	��B	��B	��B	��B	��B	�^B	�XB	�$B	��B	�xB	�dB	��B	�B	��B	�B	�	B	��B	�2B	��B	�2B	�2B	��B	�LB	�LB	��B	��B	�B	�B	�B	��B	�dB	�0B	�0B	��B	��B	��B	��B	�JB	�B	�B	�JB	�0B	��B	�JB	��B	��B	�dB	�JB	�B	��B	�.B
  B
 �B
 B	�HB	��B	�]B	�BB	��B	�cB	�B	��B	��B	��B	�B	�B	�^B	�rB	�rB	��B	��B	��B	�*B	��B	�B	�0B	�B	��B	��B	�B	��B	��B	��B	�jB	��B	��B	��B	��B	�B	��B	�BB	�(B
  B
 iB
B
B
oB
�B
 �B
B
B
 �B
 �B
 �B
 �B
  B	�]B	��B	��B	��B	�<B	��B	��B	��B	��B	��B	�.B	�HB	�cB	�cB
 �B
 �B
 �B
 �B
 �B
 �B
 �B
 B
  B	��B	��B	�wB	�wB	�(B	��B	�BB	��B
 OB
UB
�B
B
'B
�B
�B
�B
�B
SB
�B
EB
�B
	lB
	�B
	�B

�B

�B

�B
)B

�B
B
	�B
	�B
	B
�B
�B
�B
�B
�B
�B
zB
�B
�B
�B
zB
�B
KB
�B
�B
	�B

�B

�B
�B
JB
�B
�B
�B
�B
dB
PB
jB
�B
�B
"B
<B
�B
�B
�B
BB
vB
�B
�B
�B
�B
HB
�B
�B
bB
�B
�B
�B
�B
B
�B
.B
�B
�B
�B
�B
}B
�B
NB
�B
�B
�B
&B
�B
�B
�B
2B
gB
B
sB
B
�B
�B
�B
�B
�B
	B
�B
�B
B
�B
xB
CB
B
~B
�B
OB
�B
�B
 BB
 \B
!bB
!�B
!�B
!�B
"B
"B
"4B
"4B
"�B
#:B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
#B
#B
#B
#B
#TB
$B
$&B
$@B
$�B
$�B
%B
%,B
%zB
%�B
%�B
%�B
%�B
%�B
%�B
&�B
'mB
'�B
'�B
'�B
'�B
(
B
(�B
(�B
)�B
)�B
)�B
*0B
*eB
*eB
*�B
+B
+B
+QB
+kB
+�B
+�B
+�B
,B
,"B
,"B
+�B
,�B
-)B
-�B
./B
./B
-�B
.B
.IB
.�B
.�B
.�B
.�B
/5B
/OB
/�B
0!B
0UB
0�B
1[B
2aB
3MB
4�B
3�B
33B
4B
4�B
5%B
5?B
5ZB
5?B
5%B
5%B
5�B
6+B
6+B
5�B
5�B
5B
4�B
5?B
5%B
5�B
5�B
6�B
7B
7B
7LB
7�B
7�B
7�B
8lB
8�B
8lB
8B
7�B
7�B
7LB
6�B
6�B
6�B
7�B
7�B
88B
8�B
8�B
9XB
9rB
9�B
9�B
:B
:xB
:�B
:�B
;0B
;�B
<�B
=VB
=�B
>wB
?B
?}B
@OB
@iB
@�B
@�B
@�B
AUB
A�B
BB
BAB
BAB
BB
B�B
B�B
B�B
C{B
C�B
C�B
D�B
DgB
D�B
D�B
D�B
D�B
E9B
EmB
EmB
E�B
FYB
FtB
F�B
F�B
G_B
GEB
G+B
GEB
G_B
F�B
G_B
H1B
H�B
H�B
IB
I7B
IRB
I7B
J	B
J�B
KB
K)B
KxB
K^B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
LB
L0B
LdB
L�B
MB
M6B
L�B
L�B
MB
L�B
MB
M�B
M�B
M�B
M�B
NpB
N�B
N�B
N�B
OB
N�B
OBB
OB
OB
O(B
OvB
O�B
O�B
OvB
O\B
OBB
OBB
O�B
P.B
P�B
Q�B
Q�B
QhB
QNB
QNB
Q�B
Q�B
Q�B
RB
RTB
RTB
RoB
R�B
R�B
R�B
R�B
R�B
R�B
R�B
T,B
U�B
U�B
VB
VSB
W
B
W�B
W�B
W�B
XB
X_B
X_B
X_B
X_B
X_B
X_B
W�B
W�B
XyB
X�B
X�B
X�B
X�B
X�B
Y1B
YKB
Y�B
Y�B
Z�B
Z�B
Z�B
Z�B
[#B
[=B
[�B
[WB
[qB
[�B
[�B
\B
\CB
\xB
\]B
\�B
\�B
\�B
]/B
]�B
]�B
]�B
]~B
^5B
^5B
^B
^B
]�B
]�B
]�B
]~B
]/B
]B
\�B
\�B
\�B
\�B
]/B
]B
]�B
]�B
]�B
^jB
_B
_!B
_pB
_pB
_pB
_pB
_pB
_pB
_�B
_�B
_�B
_�B
`'B
`vB
`�B
aHB
a|B
a�B
a|B
a�B
a�B
a�B
a�B
b4B
bhB
b�B
cB
c:B
cTB
cnB
c�B
c�B
c�B
c�B
dB
d&B
dZB
d�B
d�B
d�B
e,B
ezB
e�B
fLB
f�B
f�B
gB
g8B
gRB
gRB
g�B
g�B
g�B
h>B
hsB
h�B
h�B
h�B
iB
i*B
i*B
iDB
iyB
i�B
jeB
j�B
k6B
kkB
k�B
k�B
k�B
l"B
l�B
l�B
l�B
l�B
mB
mB
mwB
m�B
m�B
m�B
m�B
m�B
n/B
nIB
ncB
n�B
n�B
n�B
n�B
n�B
o5B
o�B
o�B
o�B
p!B
p;B
poB
p�B
p�B
p�B
p�B
q[B
qAB
q[B
q[B
qvB
q�B
q�B
q�B
q�B
q�B
q�B
r-11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604105232  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604191528  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604191528  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604191528                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605041536  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605041536  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610151508                      G�O�G�O�G�O�                