CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:56:26Z creation;2022-06-04T17:56:26Z conversion to V3.1      
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
_FillValue                 �  I4   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M$   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p|   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tl   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �h   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �X   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �t   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ޤ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �    HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �0   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �4   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �D   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �H   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �L   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20220604175626  20220610141506  5905854                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               BA   JA                                  2B  A   APEX                            8422                            2.11.2                          846 @�JW)���1   @�JW�u�@0��S����b��l�C�1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  BxffB�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�ffB�33B�ffB���C  C  C�fC  C
  C  C  C  C  C  C  C  C  C  C  C �C"�C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT�CV  CX�CZ�C\  C^  C_�fCa�fCd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C��C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%�fD&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D��3D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @ ��@��@�Q�A (�A (�A@(�A`(�A�{A�{A�{A�{A�{A�{A�{A�{B 
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
=Bxp�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B���B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�k�B�8RB�k�B���C�C�C��C�C
�C�C�C�C�C�C�C�C�C�C�C )C")C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT)CV�CX)CZ)C\�C^�C_��Ca��Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C�HC�HC�HC�HC�C��{C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�C�HC�HC�HC�HC�HC�HC��{C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC��{C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC��{C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�C�HC�HC�HC�HC�HC�HC�HD  �D ��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D	 �D	��D
 �D
��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �Dz>D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D�>D��D  �D ��D! �D!��D" �D"��D# �D#��D$ �D$��D% �D%�
D& �D&��D' �D'��D( �D(��D) �D)��D* �D*��D+ �D+��D, �D,��D- �D-��D. �D.��D/ �D/��D0 �D0��D1 �D1��D2 �D2��D3 �D3��D4 �D4��D5 �D5��D6 �D6��D7 �D7��D8 �D8��D9 �D9��D: �D:��D; �D;��D< �D<��D= �D=��D> �D>��D? �D?��D@ �D@��DA �DA��DB �DB��DC �DC��DD �DD��DE �DE��DF �DF��DG �DG��DH �DH��DI �DI��DJ �DJ��DK �DK��DL �DL��DM �DM��DN �DN��DO �DO��DP �DP��DQ �DQ��DR �DR��DS �DS��DT �DT��DU �DU��DV �DV��DW �DW��DX �DX��DY �DY��DZ �DZ��D[ �D[��D\ �D\��D] �D]��D^ �D^��D_ �D_��D` �D`��Da �Da��Db �Db��Dc �Dc��Dd �Dd��De �De��Df �Df��Dg �Dg��Dh �Dh��Di �Di��Dj �Dj��Dk �Dk��Dl �Dl��Dm �Dm��Dn �Dn��Do �Do��Dp �Dp��Dq �Dq��Dr �Dr��Ds �Ds��Dt �Dt��Du �Du��Dv �Dv��Dw �Dw��Dx �Dx��Dy �Dy��Dz �Dz��D{ �D{��D| �D|��D} �D}��D~ �D~��D �D��D� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�=D��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�C�D��RD��RD� RD�@RD��RD��RD� RD�=D��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�C�D��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RDRD��RD� RD�@RDÀRD��RD� RD�@RDĀRD��RD� RD�@RDŀRD��RD� RD�@RDƀRD��RD� RD�@RDǀRD��RD� RD�@RDȀRD��RD� RD�@RDɀRD�ÅD� RD�@RDʀRD��RD� RD�@RDˀRD��RD� RD�@RD̀RD��RD� RD�@RD̀RD��RD� RD�@RD΀RD��RD� RD�@RDπRD��RD� RD�@RDЀRD��RD� RD�@RDрRD��RD� RD�@RDҀRD��RD� RD�@RDӀRD��RD� RD�@RDԀRD��RD� RD�@RDՀRD��RD� RD�@RDրRD��RD� RD�@RD׀RD��RD� RD�@RD؀RD��RD� RD�@RDـRD��RD� RD�@RDڀRD��RD� RD�@RDۀRD��RD� RD�@RD܀RD��RD� RD�@RD݀RD��RD� RD�@RDހRD��RD� RD�@RD߀RD��RD� RD�@RD��RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD��RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��D� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@R111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   AϘ+Aϔ{AϕAϜ�AϝAϟ�AϠ�AϢhAϣnAϥ�AϧAϨ$Aϩ*Aϧ�AϨ�AϨ�Aϩ�AϬ�Aϭ�Aϯ�Aϳ3Aϴ�Aϴ9Aϱ�Aϳ3AϺ�A϶�Aϵ?AϦAϏ�A�i�A�>A�'�A�MA��A� 4A��A��WA�бA�҉Aμ6A�m)A�A�%zA½�A�`vA��$A�rA���A�]/A�R�A�Z�A��A�U�A�a|A�MA��$A��A�a�A�$A���A��{A�,=A�_A�g�A�A�A���A���A��A���A���A�2-A���A���A��]A�:�A�O�A��wA�ĜA�JA�YA�Y�A��xA�}VA��jA�K�A��nA�m�A���A���A��A��RA���A�L�A�.A�҉A���A��UA�w�A�_�A��!A�zDA��IA�M6A��TA�a�A��pA�J�A��A�'�A�,=A�=qA���A���A�Q�A}B[Az�Ax�Aw�As?}An��Aj�Ae2�A^~(AV��AR��AQ��AP��AN7�AJ$tAC��AAp�A?�"A>W?A=��A<�A<`�A;�jA;eA:zA:Q�A:5?A9A7�^A6b�A5U2A4�*A4�A3e�A2��A1�5A1	A0Y�A/�[A/��A.�ZA-G�A+��A*A)��A)�A(��A(ZA'��A'kQA'�A%oA$U�A#ϫA"�$A!oA \�A��A.�A/A��A�]A��A<6A�$A�A��A6�A�A�4AZ�A�rAo A%�A&�AK�A�-A�A�tAc�A�A��Ap�A�A��A�1AK�An/A,=A�A��A�1AH�AA�A�*A�	AOvA��A�pAa|A@�A�A|�A�A��A�AL�A��AxA?A�"A�A%�A��A�A��AoA
�PA
0UA	�A	�A	P�A��A�2A�A��AxAu%AB[A��A�A \Al�AiDA?}A �@�@@�^5@���@���@���@��@���@�RT@�=�@��)@�u�@�%F@�s�@��@��}@��@�  @�Y�@���@��@��@�I�@�_@��@��m@�w@�I@�b@�X@�v�@�u@�*@�:�@���@�6@��'@�j@ܾ�@ܓu@�~(@�V@��@�@@�Z�@ِ�@��@٫�@��@��|@ع$@��W@ט�@׊	@�Y�@�Y�@��@�}V@�K^@��@�YK@�R�@��@���@Ӛk@�$t@��@��@���@ҦL@ҧ@�i�@��@��K@���@�w�@���@υ@�+�@η�@Ͱ�@��)@�_@�"h@��]@˜@� i@ʸR@�]d@�خ@�f�@ȷ�@�K^@�ݘ@�L�@�>B@ŝ�@�'�@��m@�kQ@�-�@��@��o@õt@��@�@�@��@�IR@��,@���@��@�zx@�8�@��L@��@�P�@��@�J�@���@��@��5@��1@�{J@��	@�҉@��j@��6@���@�V�@�%�@���@���@���@��@�YK@���@���@�6z@��4@�!@���@�F@���@�:�@��a@�a@��@�g8@�8�@�~@�@���@���@�iD@�z�@���@�o @�X@���@�7@���@�H�@���@��@�xl@�$�@��@�dZ@��@�ں@�m�@��@���@�_p@��@���@��@�V�@�.�@��@�˒@��S@�K�@�(@��$@�c�@�_@���@�{J@�>�@���@���@���@�r�@�E�@��@@�F�@�+@��@��@���@�~(@�\�@�=q@�1@��@��z@��@�=�@��@���@���@��z@�q�@�g8@�M@��g@�5�@��	@���@�$@��Z@��w@���@���@���@�y�@�RT@�"�@���@�	@���@��@��m@��
@�~�@���@�A�@�{@��@@�W?@�A�@��`@�V@�GE@�4n@���@�C�@�/@�#�@��5@���@��Y@�xl@�i�@�J�@���@���@�P�@��@��@��@�u%@�Q�@��@��@�n/@�1�@��@��[@��r@�5?@�@���@���@� \@��@��X@���@��@�V�@�M@���@�a@���@��o@�K^@�Z@�:�@���@��[@���@�rG@�'�@��|@��H@���@�tT@�'R@��
@���@�J#@�"�@���@���@���@�}V@�p;@�D�@�$�@�{@���@���@���@���@�@O@�(�@��[@�� @��A@�GE@���@���@�-w@�5?@��t@���@�n/@�j@�;d@��@�ȴ@�q�@�;�@�7�@�$�@�e@�7@��@��@��W@��@���@�zx@�\�@�g�@�W?@�9�@��@���@�tT@�Q@� @8@~ں@~�\@~�@}��@}��@|��@|�@{iD@{�:@{8@z@�@z6�@zl�@z-@x��@x/�@w33@vߤ@v�H@v��@v��@vs�@u��@u%F@tɆ@t6@sخ@sx@r��@r#:@q��@q��@q��@q+@p�@p�@o@n:*@n($@n)�@m�t@mc@l�z@lA�@l2�@l%�@ks@j҉@jkQ@jB[@i��@i�@h:�@g�K@g6z@g/�@f�H@f�L@f\�@f#:@e��@e2a@d��@c�W@cF�@cY@b��@b	@a�T@a�X@ahs@a�@`�j@`��@`?�@_�W@_{J@_Y@^��@^E�@]�@]��@]�@\�D@\h�@\Z@\9X@\!@[��@[�@[J#@[�@Z�@Z�h@Zq�@Z6�@Yԕ@Yx�@YY�@YIR@X�K@X�e@X��@X`�@XQ�@X@W��@V��@V�@U�S@U\�@U:�@U4@T��@T��@T�@TbN@S�g@Sx@SO@R��@Rs�@Q�)@Q�H@Q�t@Q��@Qj@Q(�@P�[@P�@PH@P~@O��@O!-@O�@N�@N��@N+k@Ma�@M@L�[@Lj@L@L�@L  @K��@Kخ@K�k@K�@J�m@Jl�@J&�@J	@I�'@I7L@H�z@Hj@HZ@HC-@G�]@G��@G�q@Ga@F�@F
�@E�-@E+�@D��@D�@DK^@C��@B�@B��@Bq�@BH�@A��@A��@A@@@��@@g8@@/�@?�@?��@?Z�@?9�@>��@=��@=�^@=zx@=@<�@<�D@<�@;ƨ@;�@;Mj@:��@:z@:h
@:{@9�j@9��@9(�@8��@8]d@8C-@8�@7�[@7C�@6��@5�o@5|@5Dg@5	l@4��@4>B@3�@3RT@2�@2�<@2�\@2@�@2	@1�T@1IR@1*0@0�)@0�u@0PH@0M@/��@/�V@/~�@/n/@/P�@/Y@.�R@.u%@.W�@.GE@.0U@.	@-�#@-B�@,�)@,r�@,U2@,U2@,D�@,�@+|�@+1�@+ i@*͟@*��@*s�@*0U@)��@)��@)c@)F@)�@(�@(�/@(q@(*�@'�@'��@'W?@'6z@'/�@&�@&�@&\�@%��@%2a@%�@$��@$��@$w�@$9X@$!@#��@#��@#��@#�q@#��@#~�@#qv@#]�@#$t@"�@"��@"��@"\�@"	@!�@!��@!x�@!\�@!IR@!*0@ �E@ ��@ C-@ (�@ �@��@��@��@�q@o�@�@�M@��@�F@p;@@�@8�@�@��@�N@��@��@T�@@�@|�@c�@�]@�w@��@b�@)_@��@��@_�@��@��@s�@a�@IR@8�@Ĝ@��@w�@h�@PH@<�@:�@/�@b@�m@�r@�V@��@�1@�@M�@+k@��@�@��@�3@}�@5�@�@�z@>B@'R@�@1@�g@�@P�@�@
=@��@�@i�@GE@�@�@�H@��@�7@c�@*0@@@;@��@�K@��@9X@@@�@@=@+@S111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   AϘ+Aϔ{AϕAϜ�AϝAϟ�AϠ�AϢhAϣnAϥ�AϧAϨ$Aϩ*Aϧ�AϨ�AϨ�Aϩ�AϬ�Aϭ�Aϯ�Aϳ3Aϴ�Aϴ9Aϱ�Aϳ3AϺ�A϶�Aϵ?AϦAϏ�A�i�A�>A�'�A�MA��A� 4A��A��WA�бA�҉Aμ6A�m)A�A�%zA½�A�`vA��$A�rA���A�]/A�R�A�Z�A��A�U�A�a|A�MA��$A��A�a�A�$A���A��{A�,=A�_A�g�A�A�A���A���A��A���A���A�2-A���A���A��]A�:�A�O�A��wA�ĜA�JA�YA�Y�A��xA�}VA��jA�K�A��nA�m�A���A���A��A��RA���A�L�A�.A�҉A���A��UA�w�A�_�A��!A�zDA��IA�M6A��TA�a�A��pA�J�A��A�'�A�,=A�=qA���A���A�Q�A}B[Az�Ax�Aw�As?}An��Aj�Ae2�A^~(AV��AR��AQ��AP��AN7�AJ$tAC��AAp�A?�"A>W?A=��A<�A<`�A;�jA;eA:zA:Q�A:5?A9A7�^A6b�A5U2A4�*A4�A3e�A2��A1�5A1	A0Y�A/�[A/��A.�ZA-G�A+��A*A)��A)�A(��A(ZA'��A'kQA'�A%oA$U�A#ϫA"�$A!oA \�A��A.�A/A��A�]A��A<6A�$A�A��A6�A�A�4AZ�A�rAo A%�A&�AK�A�-A�A�tAc�A�A��Ap�A�A��A�1AK�An/A,=A�A��A�1AH�AA�A�*A�	AOvA��A�pAa|A@�A�A|�A�A��A�AL�A��AxA?A�"A�A%�A��A�A��AoA
�PA
0UA	�A	�A	P�A��A�2A�A��AxAu%AB[A��A�A \Al�AiDA?}A �@�@@�^5@���@���@���@��@���@�RT@�=�@��)@�u�@�%F@�s�@��@��}@��@�  @�Y�@���@��@��@�I�@�_@��@��m@�w@�I@�b@�X@�v�@�u@�*@�:�@���@�6@��'@�j@ܾ�@ܓu@�~(@�V@��@�@@�Z�@ِ�@��@٫�@��@��|@ع$@��W@ט�@׊	@�Y�@�Y�@��@�}V@�K^@��@�YK@�R�@��@���@Ӛk@�$t@��@��@���@ҦL@ҧ@�i�@��@��K@���@�w�@���@υ@�+�@η�@Ͱ�@��)@�_@�"h@��]@˜@� i@ʸR@�]d@�خ@�f�@ȷ�@�K^@�ݘ@�L�@�>B@ŝ�@�'�@��m@�kQ@�-�@��@��o@õt@��@�@�@��@�IR@��,@���@��@�zx@�8�@��L@��@�P�@��@�J�@���@��@��5@��1@�{J@��	@�҉@��j@��6@���@�V�@�%�@���@���@���@��@�YK@���@���@�6z@��4@�!@���@�F@���@�:�@��a@�a@��@�g8@�8�@�~@�@���@���@�iD@�z�@���@�o @�X@���@�7@���@�H�@���@��@�xl@�$�@��@�dZ@��@�ں@�m�@��@���@�_p@��@���@��@�V�@�.�@��@�˒@��S@�K�@�(@��$@�c�@�_@���@�{J@�>�@���@���@���@�r�@�E�@��@@�F�@�+@��@��@���@�~(@�\�@�=q@�1@��@��z@��@�=�@��@���@���@��z@�q�@�g8@�M@��g@�5�@��	@���@�$@��Z@��w@���@���@���@�y�@�RT@�"�@���@�	@���@��@��m@��
@�~�@���@�A�@�{@��@@�W?@�A�@��`@�V@�GE@�4n@���@�C�@�/@�#�@��5@���@��Y@�xl@�i�@�J�@���@���@�P�@��@��@��@�u%@�Q�@��@��@�n/@�1�@��@��[@��r@�5?@�@���@���@� \@��@��X@���@��@�V�@�M@���@�a@���@��o@�K^@�Z@�:�@���@��[@���@�rG@�'�@��|@��H@���@�tT@�'R@��
@���@�J#@�"�@���@���@���@�}V@�p;@�D�@�$�@�{@���@���@���@���@�@O@�(�@��[@�� @��A@�GE@���@���@�-w@�5?@��t@���@�n/@�j@�;d@��@�ȴ@�q�@�;�@�7�@�$�@�e@�7@��@��@��W@��@���@�zx@�\�@�g�@�W?@�9�@��@���@�tT@�Q@� @8@~ں@~�\@~�@}��@}��@|��@|�@{iD@{�:@{8@z@�@z6�@zl�@z-@x��@x/�@w33@vߤ@v�H@v��@v��@vs�@u��@u%F@tɆ@t6@sخ@sx@r��@r#:@q��@q��@q��@q+@p�@p�@o@n:*@n($@n)�@m�t@mc@l�z@lA�@l2�@l%�@ks@j҉@jkQ@jB[@i��@i�@h:�@g�K@g6z@g/�@f�H@f�L@f\�@f#:@e��@e2a@d��@c�W@cF�@cY@b��@b	@a�T@a�X@ahs@a�@`�j@`��@`?�@_�W@_{J@_Y@^��@^E�@]�@]��@]�@\�D@\h�@\Z@\9X@\!@[��@[�@[J#@[�@Z�@Z�h@Zq�@Z6�@Yԕ@Yx�@YY�@YIR@X�K@X�e@X��@X`�@XQ�@X@W��@V��@V�@U�S@U\�@U:�@U4@T��@T��@T�@TbN@S�g@Sx@SO@R��@Rs�@Q�)@Q�H@Q�t@Q��@Qj@Q(�@P�[@P�@PH@P~@O��@O!-@O�@N�@N��@N+k@Ma�@M@L�[@Lj@L@L�@L  @K��@Kخ@K�k@K�@J�m@Jl�@J&�@J	@I�'@I7L@H�z@Hj@HZ@HC-@G�]@G��@G�q@Ga@F�@F
�@E�-@E+�@D��@D�@DK^@C��@B�@B��@Bq�@BH�@A��@A��@A@@@��@@g8@@/�@?�@?��@?Z�@?9�@>��@=��@=�^@=zx@=@<�@<�D@<�@;ƨ@;�@;Mj@:��@:z@:h
@:{@9�j@9��@9(�@8��@8]d@8C-@8�@7�[@7C�@6��@5�o@5|@5Dg@5	l@4��@4>B@3�@3RT@2�@2�<@2�\@2@�@2	@1�T@1IR@1*0@0�)@0�u@0PH@0M@/��@/�V@/~�@/n/@/P�@/Y@.�R@.u%@.W�@.GE@.0U@.	@-�#@-B�@,�)@,r�@,U2@,U2@,D�@,�@+|�@+1�@+ i@*͟@*��@*s�@*0U@)��@)��@)c@)F@)�@(�@(�/@(q@(*�@'�@'��@'W?@'6z@'/�@&�@&�@&\�@%��@%2a@%�@$��@$��@$w�@$9X@$!@#��@#��@#��@#�q@#��@#~�@#qv@#]�@#$t@"�@"��@"��@"\�@"	@!�@!��@!x�@!\�@!IR@!*0@ �E@ ��@ C-@ (�@ �@��@��@��@�q@o�@�@�M@��@�F@p;@@�@8�@�@��@�N@��@��@T�@@�@|�@c�@�]@�w@��@b�@)_@��@��@_�@��@��@s�@a�@IR@8�@Ĝ@��@w�@h�@PH@<�@:�@/�@b@�m@�r@�V@��@�1@�@M�@+k@��@�@��@�3@}�@5�@�@�z@>B@'R@�@1@�g@�@P�@�@
=@��@�@i�@GE@�@�@�H@��@�7@c�@*0@@@;@��@�K@��@9X@@@�@@=@+@S111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
�B
�B
�4B
�B
�B
cB
�B
cB
�B
� B
�iB
��B
�iB
�4B
� B
� B
�4B
�;B
�AB
��B
��B
�9B
��B
�B
�B
��B
��B
�B
��B
��B
��B
��B
�B
��B
��B
��B
�TB
یB
߾B
�B
�B
�B
ٚB
�fB
�GB
�oB
�{B
��B
��B
�#B
�|B
��B
�B
��B
�dB
��B
��B
�B
�}BdB�B(sB:�Bn/B�?B��B��B��B��B�JBԯBؓB�B��B�>B�B�B�B�B �B&fB(
B&�B%�B$&B&�B$B!�B�B�B�B�B�B�+BǮB��B�_B�PBz�Bh�B\�BM�B@4B9�B1�B)�BCB
��B
��B
�eB
w�B
UMB
?}B
($B
MB	��B	��B	ؓB	�PB	�B	��B	ncB	K)B	)�B	
	B��B��B�lB�B�[B�B�B��B�B�OB��B�RB��B	^B	�B	"hB	&�B	<B	OvB	`B	f�B	l�B	t�B	z�B	�iB	��B	�YB	�iB	��B	�6B	ĜB	רB	�B	�B	�kB	��B	��B	��B	��B
'B
 �B
B
0B

�B
�B	��B	�B	�XB	�tB	��B	�B	�
B	��B	�XB	�B	�B	� B	�bB	ބB	�B	�
B	�B	�B	��B	�xB
  B
B
�B
�B
/B
xB
�B
B
�B
kB
KB
�B
�B
B
�B
gB
{B
�B
�B
[B
�B
�B
�B
?B
mB
yB
B
�B
�B
"�B
#nB
$B
'B
'�B
'�B
'8B
&LB
$@B
!�B
!bB
#nB
!-B
�B
B
sB
mB
�B
@B
�B
�B
}B
B

�B
B
B
	�B
fB
mB
B	��B	��B	��B	�B	�pB	چB	՛B	ҽB	�TB	��B	�SB	ۦB	��B	ܬB	�QB	ؓB	��B	�FB	өB	�}B	�<B	�B	�dB	��B	�dB	�B	��B	�B	̳B	˒B	��B	�0B	�)B	�rB	��B	�SB	�OB	�HB	��B	��B	��B	�9B	�B	�nB	��B	�8B	��B	��B	�B	ĶB	ƨB	��B	ɺB	��B	��B	ɺB	�	B	��B	�(B	�}B	бB	��B	��B	�:B	��B	�MB	�B	ںB	ۦB	ݘB	ݲB	�B	��B	��B	�B	�B	��B	�B	�B	�B	�|B	�B	�NB	��B	�|B	��B	��B	�hB	�NB	��B	�NB	�B	��B	�4B	�B	�B	�B	�B	�2B	�B	�B	�RB	�XB	�0B	�B	�6B	�B	�B	��B	�CB	��B	�wB	�IB	��B	� B	�B	�5B	�!B	��B	�!B	�AB	�aB	�|B	�B	��B	�?B	�?B	�ZB	�ZB	�tB	��B	�tB	��B	��B	��B	�FB	�+B	�`B	�B	��B	�2B	��B	�8B	�RB	�rB	��B	�^B	�^B	�B	�PB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�wB	��B	��B	��B	��B	��B	��B
  B
  B
;B
UB
�B
�B
'B
B
�B
[B
{B
�B
�B
gB
MB
gB
�B
�B
�B
�B
B
�B
YB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
KB
B
�B
�B
�B
�B
�B
�B
�B
	B
	7B
	7B
	�B
	�B
	�B
	�B
	�B
	�B
	�B

=B
B
�B
�B
JB
dB
dB
dB
dB
�B
�B
�B
�B
�B
�B
�B
PB
jB
�B
�B
B
\B
B
�B
�B
�B
�B
�B
�B
B
NB
�B
�B
:B
�B
�B
B
@B
,B
,B
�B
�B
gB
gB
�B
B
SB
�B
YB
�B
�B
�B
+B
�B
�B
�B
�B
B
�B
�B
QB
�B
�B
�B
�B
WB
�B
�B
�B
]B
~B
B
OB
OB
OB
5B
OB
!B
;B
�B
�B
 'B
 \B
 BB
�B
�B
 'B
 \B
 �B
 �B
!bB
!|B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
"4B
"B
!�B
!-B
!�B
!�B
!HB
�B
dB
�B
jB
!B
pB
 BB
 �B
 �B
 �B
 �B
 �B
!B
!B
!HB
!|B
!�B
!�B
#TB
#�B
$�B
%�B
%�B
&LB
'B
&�B
&�B
&B
%�B
$�B
$ZB
$tB
$�B
$B
$B
#�B
"�B
"NB
# B
#�B
#TB
$B
$�B
%B
%�B
%,B
%�B
%�B
%�B
%�B
%�B
%zB
%`B
%zB
%�B
&2B
&B
&fB
'B
'�B
'�B
'�B
'�B
($B
($B
(�B
)*B
)�B
)�B
)�B
*KB
*eB
+�B
+�B
+�B
+�B
,=B
,�B
-wB
./B
.�B
/�B
/�B
0!B
0�B
0�B
1�B
2GB
2|B
2aB
2aB
2-B
33B
4B
4nB
4�B
4�B
4�B
5ZB
5tB
5�B
5�B
5�B
6B
6FB
6�B
6�B
72B
7B
7�B
8B
8B
8lB
8�B
9	B
9	B
9	B
9	B
9XB
9XB
9�B
9�B
9�B
:B
:B
:*B
:xB
:�B
:�B
:�B
;0B
;dB
;B
;�B
;B
;�B
;�B
<�B
=<B
=�B
=�B
=�B
=�B
>(B
>]B
>BB
>BB
>�B
>�B
?B
?HB
?�B
@OB
@OB
@iB
@�B
@�B
@�B
A B
AUB
AoB
A�B
A�B
BAB
BAB
BuB
BuB
B�B
C�B
C�B
C�B
DgB
D�B
D�B
D�B
D�B
D�B
EB
E�B
E�B
F?B
FtB
FYB
F�B
GB
G�B
G�B
G�B
HB
HKB
HfB
H�B
H�B
I7B
J#B
J#B
J�B
KB
K)B
KxB
K�B
L�B
M6B
MB
M6B
M�B
M�B
NVB
N�B
N�B
N�B
O\B
OvB
O�B
OvB
PbB
P�B
QB
Q4B
Q�B
Q�B
RB
R�B
R�B
R�B
S&B
S�B
S�B
S�B
T,B
TFB
TaB
T�B
UgB
U�B
U�B
U�B
VB
VmB
V�B
W�B
W�B
W�B
W�B
XB
X�B
X�B
YeB
Y�B
Y�B
ZB
Z�B
Z�B
Z�B
[qB
[�B
[�B
\B
\]B
\�B
\�B
]/B
]IB
]IB
]dB
]~B
]�B
]�B
^5B
^OB
^jB
^jB
^OB
^�B
_B
_VB
_�B
_�B
_�B
`BB
aB
a-B
a-B
aB
aB
a-B
a�B
bB
b4B
bhB
b�B
b�B
b�B
b�B
cTB
cnB
c�B
c�B
d@B
d&B
d&B
d@B
dtB
d�B
e�B
e�B
ffB
f�B
f�B
f�B
f�B
f�B
gB
gB
gB
g8B
g8B
gRB
gRB
g�B
h>B
hXB
h�B
h�B
h�B
i*B
i*B
iyB
i�B
i�B
i�B
i�B
jB
jKB
j�B
j�B
j�B
j�B
j�B
kB
kB
kQB
k�B
k�B
k�B
lB
l=B
lWB
lWB
lqB
l�B
l�B
l�B
l�B
mB
mCB
m�B
m�B
m�B
nIB
n�B
n�B
o B
oB
o�B
o�B
o�B
pUB
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
rB
raB
r|B
qvB
q�B
rB
q�B
q�B
q[B
q'B
q'B
q[B
rGB
rGB
r�B
sMB
s3B
r�B
r�B
r�B
r�B
r�B
sB
sB
sB
shB
s�B
s�B
tB
tnB
t�B
t�B
t�B
u%B
uZB
uZB
uZB
u�B
u�B
vB
v�B
vzB
w2B
w�B
x8B
x8B
xRB
x�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
�B
�B
�4B
�B
�B
cB
�B
cB
�B
� B
�iB
��B
�iB
�4B
� B
� B
�4B
�;B
�AB
��B
��B
�9B
��B
�B
�B
��B
��B
�B
��B
��B
��B
��B
�B
��B
��B
��B
�TB
یB
߾B
�B
�B
�B
ٚB
�fB
�GB
�oB
�{B
��B
��B
�#B
�|B
��B
�B
��B
�dB
��B
��B
�B
�}BdB�B(sB:�Bn/B�?B��B��B��B��B�JBԯBؓB�B��B�>B�B�B�B�B �B&fB(
B&�B%�B$&B&�B$B!�B�B�B�B�B�B�+BǮB��B�_B�PBz�Bh�B\�BM�B@4B9�B1�B)�BCB
��B
��B
�eB
w�B
UMB
?}B
($B
MB	��B	��B	ؓB	�PB	�B	��B	ncB	K)B	)�B	
	B��B��B�lB�B�[B�B�B��B�B�OB��B�RB��B	^B	�B	"hB	&�B	<B	OvB	`B	f�B	l�B	t�B	z�B	�iB	��B	�YB	�iB	��B	�6B	ĜB	רB	�B	�B	�kB	��B	��B	��B	��B
'B
 �B
B
0B

�B
�B	��B	�B	�XB	�tB	��B	�B	�
B	��B	�XB	�B	�B	� B	�bB	ބB	�B	�
B	�B	�B	��B	�xB
  B
B
�B
�B
/B
xB
�B
B
�B
kB
KB
�B
�B
B
�B
gB
{B
�B
�B
[B
�B
�B
�B
?B
mB
yB
B
�B
�B
"�B
#nB
$B
'B
'�B
'�B
'8B
&LB
$@B
!�B
!bB
#nB
!-B
�B
B
sB
mB
�B
@B
�B
�B
}B
B

�B
B
B
	�B
fB
mB
B	��B	��B	��B	�B	�pB	چB	՛B	ҽB	�TB	��B	�SB	ۦB	��B	ܬB	�QB	ؓB	��B	�FB	өB	�}B	�<B	�B	�dB	��B	�dB	�B	��B	�B	̳B	˒B	��B	�0B	�)B	�rB	��B	�SB	�OB	�HB	��B	��B	��B	�9B	�B	�nB	��B	�8B	��B	��B	�B	ĶB	ƨB	��B	ɺB	��B	��B	ɺB	�	B	��B	�(B	�}B	бB	��B	��B	�:B	��B	�MB	�B	ںB	ۦB	ݘB	ݲB	�B	��B	��B	�B	�B	��B	�B	�B	�B	�|B	�B	�NB	��B	�|B	��B	��B	�hB	�NB	��B	�NB	�B	��B	�4B	�B	�B	�B	�B	�2B	�B	�B	�RB	�XB	�0B	�B	�6B	�B	�B	��B	�CB	��B	�wB	�IB	��B	� B	�B	�5B	�!B	��B	�!B	�AB	�aB	�|B	�B	��B	�?B	�?B	�ZB	�ZB	�tB	��B	�tB	��B	��B	��B	�FB	�+B	�`B	�B	��B	�2B	��B	�8B	�RB	�rB	��B	�^B	�^B	�B	�PB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�wB	��B	��B	��B	��B	��B	��B
  B
  B
;B
UB
�B
�B
'B
B
�B
[B
{B
�B
�B
gB
MB
gB
�B
�B
�B
�B
B
�B
YB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
KB
B
�B
�B
�B
�B
�B
�B
�B
	B
	7B
	7B
	�B
	�B
	�B
	�B
	�B
	�B
	�B

=B
B
�B
�B
JB
dB
dB
dB
dB
�B
�B
�B
�B
�B
�B
�B
PB
jB
�B
�B
B
\B
B
�B
�B
�B
�B
�B
�B
B
NB
�B
�B
:B
�B
�B
B
@B
,B
,B
�B
�B
gB
gB
�B
B
SB
�B
YB
�B
�B
�B
+B
�B
�B
�B
�B
B
�B
�B
QB
�B
�B
�B
�B
WB
�B
�B
�B
]B
~B
B
OB
OB
OB
5B
OB
!B
;B
�B
�B
 'B
 \B
 BB
�B
�B
 'B
 \B
 �B
 �B
!bB
!|B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
"4B
"B
!�B
!-B
!�B
!�B
!HB
�B
dB
�B
jB
!B
pB
 BB
 �B
 �B
 �B
 �B
 �B
!B
!B
!HB
!|B
!�B
!�B
#TB
#�B
$�B
%�B
%�B
&LB
'B
&�B
&�B
&B
%�B
$�B
$ZB
$tB
$�B
$B
$B
#�B
"�B
"NB
# B
#�B
#TB
$B
$�B
%B
%�B
%,B
%�B
%�B
%�B
%�B
%�B
%zB
%`B
%zB
%�B
&2B
&B
&fB
'B
'�B
'�B
'�B
'�B
($B
($B
(�B
)*B
)�B
)�B
)�B
*KB
*eB
+�B
+�B
+�B
+�B
,=B
,�B
-wB
./B
.�B
/�B
/�B
0!B
0�B
0�B
1�B
2GB
2|B
2aB
2aB
2-B
33B
4B
4nB
4�B
4�B
4�B
5ZB
5tB
5�B
5�B
5�B
6B
6FB
6�B
6�B
72B
7B
7�B
8B
8B
8lB
8�B
9	B
9	B
9	B
9	B
9XB
9XB
9�B
9�B
9�B
:B
:B
:*B
:xB
:�B
:�B
:�B
;0B
;dB
;B
;�B
;B
;�B
;�B
<�B
=<B
=�B
=�B
=�B
=�B
>(B
>]B
>BB
>BB
>�B
>�B
?B
?HB
?�B
@OB
@OB
@iB
@�B
@�B
@�B
A B
AUB
AoB
A�B
A�B
BAB
BAB
BuB
BuB
B�B
C�B
C�B
C�B
DgB
D�B
D�B
D�B
D�B
D�B
EB
E�B
E�B
F?B
FtB
FYB
F�B
GB
G�B
G�B
G�B
HB
HKB
HfB
H�B
H�B
I7B
J#B
J#B
J�B
KB
K)B
KxB
K�B
L�B
M6B
MB
M6B
M�B
M�B
NVB
N�B
N�B
N�B
O\B
OvB
O�B
OvB
PbB
P�B
QB
Q4B
Q�B
Q�B
RB
R�B
R�B
R�B
S&B
S�B
S�B
S�B
T,B
TFB
TaB
T�B
UgB
U�B
U�B
U�B
VB
VmB
V�B
W�B
W�B
W�B
W�B
XB
X�B
X�B
YeB
Y�B
Y�B
ZB
Z�B
Z�B
Z�B
[qB
[�B
[�B
\B
\]B
\�B
\�B
]/B
]IB
]IB
]dB
]~B
]�B
]�B
^5B
^OB
^jB
^jB
^OB
^�B
_B
_VB
_�B
_�B
_�B
`BB
aB
a-B
a-B
aB
aB
a-B
a�B
bB
b4B
bhB
b�B
b�B
b�B
b�B
cTB
cnB
c�B
c�B
d@B
d&B
d&B
d@B
dtB
d�B
e�B
e�B
ffB
f�B
f�B
f�B
f�B
f�B
gB
gB
gB
g8B
g8B
gRB
gRB
g�B
h>B
hXB
h�B
h�B
h�B
i*B
i*B
iyB
i�B
i�B
i�B
i�B
jB
jKB
j�B
j�B
j�B
j�B
j�B
kB
kB
kQB
k�B
k�B
k�B
lB
l=B
lWB
lWB
lqB
l�B
l�B
l�B
l�B
mB
mCB
m�B
m�B
m�B
nIB
n�B
n�B
o B
oB
o�B
o�B
o�B
pUB
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
rB
raB
r|B
qvB
q�B
rB
q�B
q�B
q[B
q'B
q'B
q[B
rGB
rGB
r�B
sMB
s3B
r�B
r�B
r�B
r�B
r�B
sB
sB
sB
shB
s�B
s�B
tB
tnB
t�B
t�B
t�B
u%B
uZB
uZB
uZB
u�B
u�B
vB
v�B
vzB
w2B
w�B
x8B
x8B
xRB
x�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604105002  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604175626  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604175626  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604175626                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605025633  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605025633  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610141506                      G�O�G�O�G�O�                