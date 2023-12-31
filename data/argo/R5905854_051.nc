CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:53:41Z creation;2022-06-04T17:53:41Z conversion to V3.1      
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
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20220604175341  20220610141506  5905854                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               3A   JA                                  2B  A   APEX                            8422                            2.11.2                          846 @�$����1   @�$����0@.�$�/�c)�"��`1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @33@�  @�  A   A   AA��A`  A�  A�  A�  A�  A���A���A�33A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B���B�ffB�  B�  B�  B�  B�  B�  B�  B�  B�  B�ffB�33B�  B�  B�  B�33B�ffB�  B�  B�  B�  B�  B�  B�  B�  C   C  C�C�C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C833C:  C<  C=�fC@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj�Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dك3D��3D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�|�D��D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�
@�Q�@�Q�A (�A (�AAA`(�A�{A�{A�{A�{A��HA��HA�G�A�{B 
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
=B�B�B�B�B�B�B���B�k�B�B�B�B�B�B�B�B�B�B�k�B�8RB�B�B�B�8RB�k�B�B�B�B�B�B�B�B�C �C�C)C)C�C
�C�C�C�C�C�C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C85�C:�C<�C=��C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj)Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�C�C�C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC��{C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HD  �D ��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D	 �D	��D
 �D
��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D  �D ��D! �D!��D" �D"��D# �D#��D$ �D$��D% �D%��D& �D&��D' �D'��D( �D(��D) �D)��D* �D*��D+ �D+��D, �D,��D- �D-��D. �D.��D/ �D/��D0 �D0��D1 �D1��D2 �D2��D3 �D3��D4 �D4��D5 �D5��D6 �D6��D7 �D7��D8 �D8��D9 �D9��D: �D:��D; �D;��D< �D<��D= �D=��D> �D>��D? �D?��D@ �D@��DA �DA��DB �DB��DC �DC��DD �DD��DE �DE��DF �DF��DG �DG��DH �DH��DI �DI��DJ �DJ��DK �DK��DL �DL��DM �DM��DN �DN��DO �DO��DP �DP��DQ �DQ��DR �DR��DS �DS��DT �DT��DU �DU��DV �DV��DW �DW��DX �DX��DY �DY��DZ �DZ��D[ �D[��D\ �D\��D] �D]��D^ �D^��D_ �D_��D` �D`��Da �Da��Db �Db��Dc �Dc��Dd �Dd��De �De��Df �Df��Dg �Dg��Dh �Dh��Di �Di��Dj �Dj��Dk �Dk��Dl �Dl��Dm �Dm��Dn �Dn��Do �Do��Dp �Dp��Dq �Dq��Dr �Dr��Ds �Ds��Dt �Dt��Du �Du��Dv �Dv��Dw �Dw��Dx �Dx��Dy �Dy��Dz �Dz��D{ �D{��D| �D|��D} �D}��D~ �D~��D �D��D� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD���D��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�=D��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD���D��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RDRD��RD� RD�@RDÀRD��RD� RD�@RDĀRD��RD� RD�@RDŀRD��RD� RD�@RDƀRD��RD� RD�@RDǀRD��RD� RD�@RDȀRD��RD� RD�@RDɀRD��RD� RD�@RDʀRD��RD� RD�@RDˀRD��RD� RD�@RD̀RD��RD� RD�@RD̀RD��RD� RD�@RD΀RD��RD� RD�@RDπRD��RD� RD�@RDЀRD��RD� RD�@RDрRD��RD� RD�@RDҀRD��RD� RD�@RDӀRD��RD� RD�@RDԀRD��RD� RD�@RDՀRD��RD� RD�@RDրRD��RD� RD�@RD׀RD��RD� RD�@RD؀RD��RD� RD�@RDك�D�ÅD� RD�@RDڀRD��RD� RD�@RDۀRD��RD� RD�@RD܀RD��RD� RD�@RD݀RD��RD� RD�@RDހRD��RD� RD�@RD߀RD��RD� RD�@RD��RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�}D�D� RD�@RD��RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�=111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�JA��A��A��A�4A�:A�bA�bA��A� A��A�A�{A�A�:A� A��A��A�A�~A��A� �A� 'A�VA�A�=A��A��A��A��VA���A��5AѭA��A��Ǎ�Aˡ�AȽ�A�>wAź^A��iAĐ�A�B'A�T�A��A�F?A�>�A��A�iDA�QA�x8A��A�A�ZQA���A��aA�#�A��uA�)�A��RA��(A�%�A�IA�*0A��A��A�wfA�&�A���A���A�A�A�J�A�C�A�p;A�x�A��A���A��A�h>A�}�A�YA�H�A��ZA�IA� \A���A���A�#A�C�A�6zA}��A{�As��Ap�OAp1'Ao1'Aj�AfL0Ad�Ac;Aa�A\�rAYXyAUa�AS'�AQ
=AM��AH�VAF��AB8�A>�A<�A;�A;:*A:A:�\A9K�A7��A6��A4�A4o A3��A1��A04A/��A./A-e,A-	�A,c A+�tA*�$A'�bA%�.A#2aAn�A��ARTAhsA�6A&�A��A�A�A8A iAd�A�A��A�	A��A�A��A�A��A��Am]A�\A�AیA�#A��A��AѷA�hA|�A�A��A�A��A]dA7�A^5A��A�aA�)A�uA	A��AZ�A8A�A��A%FA	�A1�A<�A7LA��A  A	�BA	h
A	`BA	��A	xlA	6A	�AƨA:�A�#A�As�A�A�#A��A�A��A�eAB[A@OA�A"�A}VA�=A��A�A m�A YK@���@�5?@�C�@���@��@���@�6�@��d@�0�@��@��@���@�4n@��	@�&�@��@�Q�@�G�@��2@��L@�c@��@��@�[W@��@�V@�Dg@�<@��@���@�`�@�?}@�4@�G�@�J@�9X@�q@�Ft@��@��@�/�@��@�Q�@��@�~�@�s�@�m�@��]@�O@�<�@��@޵�@�خ@��@�4@�v�@܌@�[�@�u%@�q�@�=q@�
�@ڰ!@ټ�@ػ�@��j@�A�@��H@�˒@�%F@��@Ӵ�@�(�@ҡb@�1@ѻ0@љ�@�9�@��@У@���@�t�@� i@�%@��@�Mj@�n/@��	@ά@΄�@�#:@��@͂�@�!�@���@̨�@�(�@�,=@��.@˼�@�~�@�n/@���@�-�@�=�@�U2@��#@�B�@Ɓo@�~@��
@ŖS@�{J@�^�@��2@ļj@ēu@�$@��Q@ßV@Å@�u�@�[W@�-w@���@�u�@�C�@��@��
@�_p@��]@��@��9@�]d@�C�@��@�c�@�L�@�U�@�a�@�G�@�S@��D@�$@���@�s@�G�@��`@���@�S�@��@���@�H�@�ی@��r@�@��:@�@O@��"@�g8@�/�@��@�[W@���@���@�_�@�1�@��+@�s@��@�PH@��@��q@�p�@�/@���@�u@���@�o�@�_p@�@@�,=@�ԕ@�P�@��@���@��U@�`�@��@��k@��	@�&�@��@�C�@���@�xl@���@��@��_@�.�@���@��F@�X@��@��!@�@��@���@��-@�Dg@���@�4n@��@�u�@��@��L@�+k@���@���@��h@�P�@�@��O@��.@�Xy@�C-@���@�zx@�O�@��@���@��1@�$�@�s@���@�Ov@��@��A@���@���@��@�Ĝ@��_@�1�@��@���@��~@�Y�@�)_@�@���@�v�@�9X@��X@�Mj@�33@��@�?�@��@� �@��]@���@��{@��f@�r�@�-@��
@���@��{@�G�@���@��E@��u@�n�@�Z@�4n@��@��F@��S@��f@�m]@�8@���@���@���@�^5@��@���@�~�@�j@�K�@�(�@�֡@�Ov@� �@��@�:�@��|@�%@��[@���@�ȴ@��@�Ov@�0U@�J@�خ@��H@��"@���@�m�@�7@���@�l�@�IR@��@���@�l"@�]d@�M@�|�@�+@���@�֡@���@�q�@�a|@�'R@���@���@���@��I@���@�_@��>@�m]@�=@��	@��/@��L@�w�@��@��Z@�e�@�9�@��@���@���@�-�@��@��@>�@~@�@}�@}8�@|��@|	�@{X�@zߤ@zxl@y�D@yc@x��@xU2@x!@w��@w�P@w4�@w�@vߤ@vu%@v@u��@uk�@uN<@t�)@t�.@t2�@t,=@t$@s��@s|�@r��@rkQ@r?@r�@qc@q8�@q	l@p�@p�?@pĜ@p��@pu�@p	�@o��@oK�@o i@n��@n	@l�	@l  @k�Q@k��@k�4@kb�@kA�@j�<@jff@jC�@j8�@jJ@i�^@iL�@i(�@i;@hy>@h?�@h%�@gb�@f�H@fp;@f�@e�@e|@d��@c�W@c�$@cg�@b�@b�x@b;�@a�@a�@a�C@ac�@`��@`�@_��@_s@^�,@^}V@^a|@^�@]x�@]c�@]X@]L�@]4@\�`@\C-@[�@[C�@Z}V@Y�n@YL�@X�4@XXy@X�@W�g@W��@W+@Vi�@VR�@V��@Va|@VR�@V.�@V4@U�>@U�@U|@UA @U�@T�`@T�@Th�@S�&@SC�@SA�@S�@R�L@Rs�@R� @Rff@Q�T@Qm]@Q:�@Q��@Q��@Q[W@Q?}@Q%F@P�5@P��@P�u@PtT@PS�@P@O�g@O��@O9�@O�@N�@NJ�@M�3@M�n@MO�@L��@LtT@L4n@K�@K�[@KC�@J�L@JL0@J�@I�@I��@Iq@H�f@H�@H��@H,=@G�@Gx@G(@FL0@F	@E�>@E�z@Ec�@D�@Dy>@C�m@C��@Cy�@C+@B��@B}V@BGE@A�@Ac@A�@@h�@?�m@?�$@?g�@?C�@?&@>��@>h
@=�@=��@=G�@<�5@<�$@<m�@<I�@;ݘ@;U�@;/�@;+@:�"@:��@:{�@:^5@:8�@9�@9�H@9�h@9T�@9+@8�)@8N�@7ݘ@7�0@7�:@7qv@7�@6�6@6kQ@6\�@5�@4�P@4�_@4�@4Ft@4@3��@3J#@2�@2�@2��@2kQ@2&�@1�@1u�@1&�@0��@0��@01'@/��@/b�@/!-@.�@.��@.q�@.#:@-�@-k�@-B�@-	l@,��@,�@,�_@,y>@+�@+�@+��@+��@+;d@+(@*�,@*�!@*�}@*�@*�s@*��@*-@)��@)+�@)@(�f@(�@(��@(c�@(c�@(I�@(�@'�:@'
=@&�x@&$�@%�o@%��@%��@%|@$�@$��@$%�@#�A@#�w@#��@#F�@#C@"͟@"L0@!�>@!�"@!T�@!%F@ ��@ ��@ z�@ u�@ j@ C-@��@y�@.I@�@��@��@kQ@ �@�@�n@X@:�@�@�@��@�.@l"@D�@7@�q@iD@W?@$t@�!@W�@C�@1�@u@��@�@k�@?}@�@�@�@�$@��@�o@e�@1@��@��@j�@U�@C�@��@�@ff@\�@8�@{@�@��@�=@^�@�@֡@�9@��@��@:�@G@�g@��@��@t�@b�@C�@�@�8@�H@��@�'@��@�A@�@�)@��@��@m]@j@hs@N<@*0@q@@@%@�K@�@r�@9X@�g@|�@�@�M@�y@�]@ȴ@��@�A@@�^@�@f�@G�@4@V@��@��@�z@��@[�@C-@"h@�m@�
@��@|�@s@_p111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�JA��A��A��A�4A�:A�bA�bA��A� A��A�A�{A�A�:A� A��A��A�A�~A��A� �A� 'A�VA�A�=A��A��A��A��VA���A��5AѭA��A��Ǎ�Aˡ�AȽ�A�>wAź^A��iAĐ�A�B'A�T�A��A�F?A�>�A��A�iDA�QA�x8A��A�A�ZQA���A��aA�#�A��uA�)�A��RA��(A�%�A�IA�*0A��A��A�wfA�&�A���A���A�A�A�J�A�C�A�p;A�x�A��A���A��A�h>A�}�A�YA�H�A��ZA�IA� \A���A���A�#A�C�A�6zA}��A{�As��Ap�OAp1'Ao1'Aj�AfL0Ad�Ac;Aa�A\�rAYXyAUa�AS'�AQ
=AM��AH�VAF��AB8�A>�A<�A;�A;:*A:A:�\A9K�A7��A6��A4�A4o A3��A1��A04A/��A./A-e,A-	�A,c A+�tA*�$A'�bA%�.A#2aAn�A��ARTAhsA�6A&�A��A�A�A8A iAd�A�A��A�	A��A�A��A�A��A��Am]A�\A�AیA�#A��A��AѷA�hA|�A�A��A�A��A]dA7�A^5A��A�aA�)A�uA	A��AZ�A8A�A��A%FA	�A1�A<�A7LA��A  A	�BA	h
A	`BA	��A	xlA	6A	�AƨA:�A�#A�As�A�A�#A��A�A��A�eAB[A@OA�A"�A}VA�=A��A�A m�A YK@���@�5?@�C�@���@��@���@�6�@��d@�0�@��@��@���@�4n@��	@�&�@��@�Q�@�G�@��2@��L@�c@��@��@�[W@��@�V@�Dg@�<@��@���@�`�@�?}@�4@�G�@�J@�9X@�q@�Ft@��@��@�/�@��@�Q�@��@�~�@�s�@�m�@��]@�O@�<�@��@޵�@�خ@��@�4@�v�@܌@�[�@�u%@�q�@�=q@�
�@ڰ!@ټ�@ػ�@��j@�A�@��H@�˒@�%F@��@Ӵ�@�(�@ҡb@�1@ѻ0@љ�@�9�@��@У@���@�t�@� i@�%@��@�Mj@�n/@��	@ά@΄�@�#:@��@͂�@�!�@���@̨�@�(�@�,=@��.@˼�@�~�@�n/@���@�-�@�=�@�U2@��#@�B�@Ɓo@�~@��
@ŖS@�{J@�^�@��2@ļj@ēu@�$@��Q@ßV@Å@�u�@�[W@�-w@���@�u�@�C�@��@��
@�_p@��]@��@��9@�]d@�C�@��@�c�@�L�@�U�@�a�@�G�@�S@��D@�$@���@�s@�G�@��`@���@�S�@��@���@�H�@�ی@��r@�@��:@�@O@��"@�g8@�/�@��@�[W@���@���@�_�@�1�@��+@�s@��@�PH@��@��q@�p�@�/@���@�u@���@�o�@�_p@�@@�,=@�ԕ@�P�@��@���@��U@�`�@��@��k@��	@�&�@��@�C�@���@�xl@���@��@��_@�.�@���@��F@�X@��@��!@�@��@���@��-@�Dg@���@�4n@��@�u�@��@��L@�+k@���@���@��h@�P�@�@��O@��.@�Xy@�C-@���@�zx@�O�@��@���@��1@�$�@�s@���@�Ov@��@��A@���@���@��@�Ĝ@��_@�1�@��@���@��~@�Y�@�)_@�@���@�v�@�9X@��X@�Mj@�33@��@�?�@��@� �@��]@���@��{@��f@�r�@�-@��
@���@��{@�G�@���@��E@��u@�n�@�Z@�4n@��@��F@��S@��f@�m]@�8@���@���@���@�^5@��@���@�~�@�j@�K�@�(�@�֡@�Ov@� �@��@�:�@��|@�%@��[@���@�ȴ@��@�Ov@�0U@�J@�خ@��H@��"@���@�m�@�7@���@�l�@�IR@��@���@�l"@�]d@�M@�|�@�+@���@�֡@���@�q�@�a|@�'R@���@���@���@��I@���@�_@��>@�m]@�=@��	@��/@��L@�w�@��@��Z@�e�@�9�@��@���@���@�-�@��@��@>�@~@�@}�@}8�@|��@|	�@{X�@zߤ@zxl@y�D@yc@x��@xU2@x!@w��@w�P@w4�@w�@vߤ@vu%@v@u��@uk�@uN<@t�)@t�.@t2�@t,=@t$@s��@s|�@r��@rkQ@r?@r�@qc@q8�@q	l@p�@p�?@pĜ@p��@pu�@p	�@o��@oK�@o i@n��@n	@l�	@l  @k�Q@k��@k�4@kb�@kA�@j�<@jff@jC�@j8�@jJ@i�^@iL�@i(�@i;@hy>@h?�@h%�@gb�@f�H@fp;@f�@e�@e|@d��@c�W@c�$@cg�@b�@b�x@b;�@a�@a�@a�C@ac�@`��@`�@_��@_s@^�,@^}V@^a|@^�@]x�@]c�@]X@]L�@]4@\�`@\C-@[�@[C�@Z}V@Y�n@YL�@X�4@XXy@X�@W�g@W��@W+@Vi�@VR�@V��@Va|@VR�@V.�@V4@U�>@U�@U|@UA @U�@T�`@T�@Th�@S�&@SC�@SA�@S�@R�L@Rs�@R� @Rff@Q�T@Qm]@Q:�@Q��@Q��@Q[W@Q?}@Q%F@P�5@P��@P�u@PtT@PS�@P@O�g@O��@O9�@O�@N�@NJ�@M�3@M�n@MO�@L��@LtT@L4n@K�@K�[@KC�@J�L@JL0@J�@I�@I��@Iq@H�f@H�@H��@H,=@G�@Gx@G(@FL0@F	@E�>@E�z@Ec�@D�@Dy>@C�m@C��@Cy�@C+@B��@B}V@BGE@A�@Ac@A�@@h�@?�m@?�$@?g�@?C�@?&@>��@>h
@=�@=��@=G�@<�5@<�$@<m�@<I�@;ݘ@;U�@;/�@;+@:�"@:��@:{�@:^5@:8�@9�@9�H@9�h@9T�@9+@8�)@8N�@7ݘ@7�0@7�:@7qv@7�@6�6@6kQ@6\�@5�@4�P@4�_@4�@4Ft@4@3��@3J#@2�@2�@2��@2kQ@2&�@1�@1u�@1&�@0��@0��@01'@/��@/b�@/!-@.�@.��@.q�@.#:@-�@-k�@-B�@-	l@,��@,�@,�_@,y>@+�@+�@+��@+��@+;d@+(@*�,@*�!@*�}@*�@*�s@*��@*-@)��@)+�@)@(�f@(�@(��@(c�@(c�@(I�@(�@'�:@'
=@&�x@&$�@%�o@%��@%��@%|@$�@$��@$%�@#�A@#�w@#��@#F�@#C@"͟@"L0@!�>@!�"@!T�@!%F@ ��@ ��@ z�@ u�@ j@ C-@��@y�@.I@�@��@��@kQ@ �@�@�n@X@:�@�@�@��@�.@l"@D�@7@�q@iD@W?@$t@�!@W�@C�@1�@u@��@�@k�@?}@�@�@�@�$@��@�o@e�@1@��@��@j�@U�@C�@��@�@ff@\�@8�@{@�@��@�=@^�@�@֡@�9@��@��@:�@G@�g@��@��@t�@b�@C�@�@�8@�H@��@�'@��@�A@�@�)@��@��@m]@j@hs@N<@*0@q@@@%@�K@�@r�@9X@�g@|�@�@�M@�y@�]@ȴ@��@�A@@�^@�@f�@G�@4@V@��@��@�z@��@[�@C-@"h@�m@�
@��@|�@s@_p111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B	3�B	3�B	4B	4B	3�B	3�B	3�B	4B	4B	4B	4B	4B	4B	4B	4TB	4TB	4B	4B	3�B	3�B	3�B	4B	3�B	3�B	3�B	3MB	3�B	2�B	1�B	1vB	0�B	/�B	,�B	 �B	�B	�B	?B�B�B	aB	gB	�B	�B	�B	 B	(sB	6�B	A�B	��B	�IB
E�B
j�B
��B%�B.�BiDBm�BtBd�B<�B?BE�BD�BC�BE�BL0BhsBb�B\�BsB|6B��Bs�BR�B4�BB
��B
�2B
ϫB
��B
�B
�B
��B
o�B
]B
6B
"�B
)B
:B
 �B	�B	��B	�,B	��B	�B	�oB	iDB	M�B	EB	=�B	5ZB	)*B	
B	�B�dB�B�hB�B�BںBڠB޸B�B�0B��B	�B	}B	�B	�B	[B	(�B	9�B	1�B	+B	+QB	2�B	<6B	>�B	CaB	FYB	JrB	=�B	.�B	�B	�B��B	  B	 B	�B	2�B	IRB	i�B	yXB	}B	v�B	w�B	}�B	� B	��B	q'B	]IB	W�B	RB	G�B	G�B	?�B	H1B	VSB	_;B	`�B	`�B	`�B	ZkB	bB	z�B	�\B	��B	��B	�8B	��B	�B	�2B	��B	āB	�?B	ɆB	͟B	��B	��B	͹B	��B	͹B	�7B	�^B	�BB	��B	ϑB	�vB	��B	��B	��B	�B	��B	�@B	�TB	�:B	�,B	خB	��B	�
B	ևB	�{B	�B	ԯB	��B	�NB	�B	�B	��B	�+B	�EB	�SB	�+B	��B	ʦB	ňB	�B	�aB	�iB	�UB	��B	�(B	�VB	�6B	��B	�JB	��B	ÖB	��B	�B	��B	�FB	ˬB	бB	��B	�\B	�<B	�B	ΥB	� B	�VB	�~B	�dB	�DB	�XB	�=B	ΊB	�gB	�TB	�JB	�+B	ƎB	ˬB	ΊB	��B	̳B	�1B	��B	�QB	ٚB	�_B	רB	ՁB	��B	�EB	֡B	өB	�NB	ҽB	�oB	��B	�B	҉B	ּB	��B	�_B	�KB	�B	��B	�B	�gB	� B	ΥB	�(B	�[B	�BB	��B	�<B	�pB	��B	�bB	ЗB	ЗB	��B	�B	��B	�aB	ևB	خB	�eB	��B	ںB	�CB	�B	�B	�jB	ݘB	ܬB	�B	��B	��B	�B	ޞB	�bB	�B	�B	�B	�B	�B	�B	�yB	�B	�B	�nB	�@B	�,B	�B	�8B	�mB	�B	�RB	��B	�fB	��B	�mB	�*B	�B	��B	��B	�0B	�B	�B	��B	�B	�"B	�=B	��B	�B	�WB	�B	�B	�6B	�B	�QB	�kB	�B	��B	��B	�B	��B	��B	�=B	�CB	�wB	��B	�/B	�IB	�cB	�cB	��B	�iB	��B	�B	��B	�AB	��B	�B	�-B	�|B	�MB	�9B	�nB	��B	��B	��B	�B	�FB	��B	�?B	��B	�B	��B	�8B	��B	��B	�LB	�2B	�2B	��B	��B	�zB	��B	��B	��B	�DB	��B	�6B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	�zB	�FB	��B	�lB	�XB	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	�<B	��B	��B	�BB
 4B
;B
;B
�B
�B
�B
�B
-B
�B
3B
B
�B
�B
zB
�B
	lB

	B

	B

#B

XB

rB
DB
DB
B
�B
B
B
B
dB
~B
dB
�B
jB
�B
�B
B
B
vB
�B
B
:B
 B
oB
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
MB
MB
�B
�B
�B
�B

B
�B
$B
�B
B
�B
�B
B
B
�B
B
B
kB
�B
#B
�B
�B
WB
)B
�B
/B
~B
�B
�B
OB
�B
�B
B
;B
B
 \B
�B
�B
!bB
!bB
!�B
!�B
"�B
#nB
#�B
#�B
$@B
%FB
%�B
%�B
%�B
%�B
&2B
&2B
&�B
'B
'B
'�B
&�B
&�B
&2B
%�B
$�B
$�B
$tB
%FB
$�B
$�B
&�B
&B
%zB
&2B
&�B
(
B
(�B
'�B
(�B
*eB
*�B
)�B
)�B
)�B
*B
)�B
)�B
*B
*0B
*�B
,"B
,"B
,B
+�B
,B
,WB
,�B
,�B
,�B
-)B
-]B
-�B
./B
./B
.�B
.�B
.�B
/5B
/iB
/�B
/�B
/�B
0!B
1B
1'B
1'B
1vB
1�B
1�B
2-B
2aB
2aB
2�B
3B
33B
3hB
33B
4B
3�B
4B
3�B
3�B
3�B
4B
4B
4B
4�B
4�B
4�B
4�B
5?B
5�B
5�B
5�B
6+B
6�B
72B
7B
7�B
7�B
8lB
9	B
8�B
:*B
:xB
;�B
<jB
<�B
=<B
=<B
=qB
=�B
=�B
=�B
=�B
=�B
=�B
>wB
>wB
>�B
>wB
>�B
?.B
>�B
?B
?.B
?HB
?cB
@ B
@4B
@�B
@�B
A;B
@�B
@�B
A B
AoB
A�B
A�B
BB
BB
B�B
C-B
DB
DMB
DgB
D�B
D�B
E9B
ESB
E�B
E9B
EB
EB
EmB
ESB
EB
DgB
D�B
D�B
E�B
E�B
GEB
G�B
G_B
G_B
G�B
IlB
J=B
JXB
J=B
J#B
J	B
JXB
J�B
J�B
J�B
J�B
J�B
J�B
K)B
KxB
K�B
K�B
K�B
L�B
L�B
MPB
MjB
M�B
M�B
M�B
N<B
N�B
N�B
N�B
OBB
O\B
O�B
O�B
O�B
O�B
P�B
P}B
P�B
P�B
Q B
P�B
QhB
Q4B
Q�B
Q4B
Q B
PHB
O�B
PbB
P�B
Q4B
Q B
P�B
Q B
Q4B
QNB
Q�B
R�B
R�B
S@B
SuB
S�B
S�B
S�B
T{B
T�B
T�B
U�B
U�B
U�B
VB
VSB
V�B
V�B
V�B
V�B
W$B
WsB
WsB
W�B
W�B
W�B
W�B
W�B
XEB
X_B
Y1B
YB
Y�B
Y�B
Y�B
ZB
ZkB
ZkB
ZQB
Z�B
[qB
[�B
[�B
[�B
[�B
\CB
\�B
\�B
\�B
\�B
]B
]IB
]�B
^5B
^�B
^�B
^�B
_!B
_;B
_pB
_�B
_�B
_�B
`B
`�B
`�B
a-B
a|B
a|B
abB
a�B
bB
bNB
b�B
cTB
cTB
c:B
c B
c:B
c B
cTB
cnB
c�B
c�B
d�B
c�B
dB
dZB
dtB
dtB
d�B
e,B
e�B
e�B
f�B
gRB
g�B
hXB
h�B
h�B
h�B
h�B
h�B
h�B
i�B
i�B
jB
j�B
kB
kB
kkB
k�B
k�B
lWB
l�B
l�B
mCB
mwB
m�B
nB
n/B
n/B
nB
n/B
n}B
n�B
o B
o B
oB
o5B
o�B
o�B
pB
p!B
poB
poB
p�B
p�B
p�B
p�B
p�B
q'B
qAB
q�B
q�B
q�B
rB
r�B
r�B
r�B
r�B
sB
sMB
s3B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
sB
r�B
s3B
sMB
shB
s�B
t9B
tnB
t�B
t�B
tnB
tTB
t�B
t�B
t�B
t�B
t�B
uB
t�B
u%B
uZB
utB
u�B
u�B
u�B
vB
vB
vFB
v�B
vzB
v�B
v�B
v�B
v�B
v�B
wLB
wfB
w�B
w�B
w�B
w�B
w�B
xB
xRB
xRB
xRB
x8B
xlB
x�B
x�B
y	B
y�B
y�B
zDB
zDB
z^B
z^B
zxB
z�B
z�B
{B
{B
{�B
{�B
{�B
{�B
|B
|B
|PB
|jB
|PB
|�B
|�B
}B
}<B
}"B
}�B
}�B
}�B
}�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B	3�B	3�B	4B	4B	3�B	3�B	3�B	4B	4B	4B	4B	4B	4B	4B	4TB	4TB	4B	4B	3�B	3�B	3�B	4B	3�B	3�B	3�B	3MB	3�B	2�B	1�B	1vB	0�B	/�B	,�B	 �B	�B	�B	?B�B�B	aB	gB	�B	�B	�B	 B	(sB	6�B	A�B	��B	�IB
E�B
j�B
��B%�B.�BiDBm�BtBd�B<�B?BE�BD�BC�BE�BL0BhsBb�B\�BsB|6B��Bs�BR�B4�BB
��B
�2B
ϫB
��B
�B
�B
��B
o�B
]B
6B
"�B
)B
:B
 �B	�B	��B	�,B	��B	�B	�oB	iDB	M�B	EB	=�B	5ZB	)*B	
B	�B�dB�B�hB�B�BںBڠB޸B�B�0B��B	�B	}B	�B	�B	[B	(�B	9�B	1�B	+B	+QB	2�B	<6B	>�B	CaB	FYB	JrB	=�B	.�B	�B	�B��B	  B	 B	�B	2�B	IRB	i�B	yXB	}B	v�B	w�B	}�B	� B	��B	q'B	]IB	W�B	RB	G�B	G�B	?�B	H1B	VSB	_;B	`�B	`�B	`�B	ZkB	bB	z�B	�\B	��B	��B	�8B	��B	�B	�2B	��B	āB	�?B	ɆB	͟B	��B	��B	͹B	��B	͹B	�7B	�^B	�BB	��B	ϑB	�vB	��B	��B	��B	�B	��B	�@B	�TB	�:B	�,B	خB	��B	�
B	ևB	�{B	�B	ԯB	��B	�NB	�B	�B	��B	�+B	�EB	�SB	�+B	��B	ʦB	ňB	�B	�aB	�iB	�UB	��B	�(B	�VB	�6B	��B	�JB	��B	ÖB	��B	�B	��B	�FB	ˬB	бB	��B	�\B	�<B	�B	ΥB	� B	�VB	�~B	�dB	�DB	�XB	�=B	ΊB	�gB	�TB	�JB	�+B	ƎB	ˬB	ΊB	��B	̳B	�1B	��B	�QB	ٚB	�_B	רB	ՁB	��B	�EB	֡B	өB	�NB	ҽB	�oB	��B	�B	҉B	ּB	��B	�_B	�KB	�B	��B	�B	�gB	� B	ΥB	�(B	�[B	�BB	��B	�<B	�pB	��B	�bB	ЗB	ЗB	��B	�B	��B	�aB	ևB	خB	�eB	��B	ںB	�CB	�B	�B	�jB	ݘB	ܬB	�B	��B	��B	�B	ޞB	�bB	�B	�B	�B	�B	�B	�B	�yB	�B	�B	�nB	�@B	�,B	�B	�8B	�mB	�B	�RB	��B	�fB	��B	�mB	�*B	�B	��B	��B	�0B	�B	�B	��B	�B	�"B	�=B	��B	�B	�WB	�B	�B	�6B	�B	�QB	�kB	�B	��B	��B	�B	��B	��B	�=B	�CB	�wB	��B	�/B	�IB	�cB	�cB	��B	�iB	��B	�B	��B	�AB	��B	�B	�-B	�|B	�MB	�9B	�nB	��B	��B	��B	�B	�FB	��B	�?B	��B	�B	��B	�8B	��B	��B	�LB	�2B	�2B	��B	��B	�zB	��B	��B	��B	�DB	��B	�6B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	�zB	�FB	��B	�lB	�XB	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	�<B	��B	��B	�BB
 4B
;B
;B
�B
�B
�B
�B
-B
�B
3B
B
�B
�B
zB
�B
	lB

	B

	B

#B

XB

rB
DB
DB
B
�B
B
B
B
dB
~B
dB
�B
jB
�B
�B
B
B
vB
�B
B
:B
 B
oB
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
MB
MB
�B
�B
�B
�B

B
�B
$B
�B
B
�B
�B
B
B
�B
B
B
kB
�B
#B
�B
�B
WB
)B
�B
/B
~B
�B
�B
OB
�B
�B
B
;B
B
 \B
�B
�B
!bB
!bB
!�B
!�B
"�B
#nB
#�B
#�B
$@B
%FB
%�B
%�B
%�B
%�B
&2B
&2B
&�B
'B
'B
'�B
&�B
&�B
&2B
%�B
$�B
$�B
$tB
%FB
$�B
$�B
&�B
&B
%zB
&2B
&�B
(
B
(�B
'�B
(�B
*eB
*�B
)�B
)�B
)�B
*B
)�B
)�B
*B
*0B
*�B
,"B
,"B
,B
+�B
,B
,WB
,�B
,�B
,�B
-)B
-]B
-�B
./B
./B
.�B
.�B
.�B
/5B
/iB
/�B
/�B
/�B
0!B
1B
1'B
1'B
1vB
1�B
1�B
2-B
2aB
2aB
2�B
3B
33B
3hB
33B
4B
3�B
4B
3�B
3�B
3�B
4B
4B
4B
4�B
4�B
4�B
4�B
5?B
5�B
5�B
5�B
6+B
6�B
72B
7B
7�B
7�B
8lB
9	B
8�B
:*B
:xB
;�B
<jB
<�B
=<B
=<B
=qB
=�B
=�B
=�B
=�B
=�B
=�B
>wB
>wB
>�B
>wB
>�B
?.B
>�B
?B
?.B
?HB
?cB
@ B
@4B
@�B
@�B
A;B
@�B
@�B
A B
AoB
A�B
A�B
BB
BB
B�B
C-B
DB
DMB
DgB
D�B
D�B
E9B
ESB
E�B
E9B
EB
EB
EmB
ESB
EB
DgB
D�B
D�B
E�B
E�B
GEB
G�B
G_B
G_B
G�B
IlB
J=B
JXB
J=B
J#B
J	B
JXB
J�B
J�B
J�B
J�B
J�B
J�B
K)B
KxB
K�B
K�B
K�B
L�B
L�B
MPB
MjB
M�B
M�B
M�B
N<B
N�B
N�B
N�B
OBB
O\B
O�B
O�B
O�B
O�B
P�B
P}B
P�B
P�B
Q B
P�B
QhB
Q4B
Q�B
Q4B
Q B
PHB
O�B
PbB
P�B
Q4B
Q B
P�B
Q B
Q4B
QNB
Q�B
R�B
R�B
S@B
SuB
S�B
S�B
S�B
T{B
T�B
T�B
U�B
U�B
U�B
VB
VSB
V�B
V�B
V�B
V�B
W$B
WsB
WsB
W�B
W�B
W�B
W�B
W�B
XEB
X_B
Y1B
YB
Y�B
Y�B
Y�B
ZB
ZkB
ZkB
ZQB
Z�B
[qB
[�B
[�B
[�B
[�B
\CB
\�B
\�B
\�B
\�B
]B
]IB
]�B
^5B
^�B
^�B
^�B
_!B
_;B
_pB
_�B
_�B
_�B
`B
`�B
`�B
a-B
a|B
a|B
abB
a�B
bB
bNB
b�B
cTB
cTB
c:B
c B
c:B
c B
cTB
cnB
c�B
c�B
d�B
c�B
dB
dZB
dtB
dtB
d�B
e,B
e�B
e�B
f�B
gRB
g�B
hXB
h�B
h�B
h�B
h�B
h�B
h�B
i�B
i�B
jB
j�B
kB
kB
kkB
k�B
k�B
lWB
l�B
l�B
mCB
mwB
m�B
nB
n/B
n/B
nB
n/B
n}B
n�B
o B
o B
oB
o5B
o�B
o�B
pB
p!B
poB
poB
p�B
p�B
p�B
p�B
p�B
q'B
qAB
q�B
q�B
q�B
rB
r�B
r�B
r�B
r�B
sB
sMB
s3B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
sB
r�B
s3B
sMB
shB
s�B
t9B
tnB
t�B
t�B
tnB
tTB
t�B
t�B
t�B
t�B
t�B
uB
t�B
u%B
uZB
utB
u�B
u�B
u�B
vB
vB
vFB
v�B
vzB
v�B
v�B
v�B
v�B
v�B
wLB
wfB
w�B
w�B
w�B
w�B
w�B
xB
xRB
xRB
xRB
x8B
xlB
x�B
x�B
y	B
y�B
y�B
zDB
zDB
z^B
z^B
zxB
z�B
z�B
{B
{B
{�B
{�B
{�B
{�B
|B
|B
|PB
|jB
|PB
|�B
|�B
}B
}<B
}"B
}�B
}�B
}�B
}�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104955  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604175341  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604175341  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604175341                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605025348  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605025349  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610141506                      G�O�G�O�G�O�                