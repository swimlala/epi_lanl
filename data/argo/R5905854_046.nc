CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:52:46Z creation;2022-06-04T17:52:46Z conversion to V3.1      
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
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20220604175246  20220610141505  5905854                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               .A   JA                                  2B  A   APEX                            8422                            2.11.2                          846 @����	1   @��-!�@0�l�C���cc��S��1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @�  @�  A��A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  BxffB�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  Bԙ�B�  B���B�  B䙚B�  B�  B�  B�33B�  B���B���C  C  C  C  C	�fC  C  CL�C�C��C�fC  C  C  C  C   C"  C$  C&  C(  C)�fC,  C.  C0  C2  C4  C6�C8L�C:  C;��C=�fC@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch�Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)�fD*  D*� D+  D+� D,  D,� D-  D-� D.fD.�fD/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�<�Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�Q�@�Q�AA (�A@(�A`(�A�{A�{A�{A�{A�{A�{A�{A�{B 
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
=Bxp�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�BԞ�B�B���B�B䞹B�B�B�B�8RB�B���B���C�C�C�C�C	��C�C�CO\C)C�\C��C�C�C�C�C �C"�C$�C&�C(�C)��C,�C.�C0�C2�C4�C6)C8O\C:�C;�\C=��C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch)Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC��{C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HD  �D ��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D	 �D	��D
 �D
��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D  �D ��D! �D!��D" �D"��D# �D#��D$ �D$��D% �D%��D& �D&��D' �D'��D( �D(��D) �D)�
D* �D*��D+ �D+��D, �D,��D- �D-��D.
D.�
D/ �D/��D0 �D0��D1 �D1��D2 �D2��D3 �D3��D4 �D4��D5 �D5��D6 �D6��D7 �D7��D8 �D8��D9 �D9��D: �D:��D; �D;��D< �D<��D= �D=��D> �D>��D? �D?��D@ �D@��DA �DA��DB �DB��DC �DC��DD �DD��DE �DE��DF �DF��DG �DG��DH �DH��DI �DI��DJ �DJ��DK �DK��DL �DL��DM �DM��DN �DN��DO �DO��DP �DP��DQ �DQ��DR �DR��DS �DS��DT �DT��DU �DU��DV �DV��DW �DW��DX �DX��DY �DY��DZ �DZ��D[ �D[��D\ �D\��D] �D]��D^ �D^��D_ �D_��D` �D`��Da �Da��Db �Db��Dc �Dc��Dd �Dd��De �De��Df �Df��Dg �Dg��Dh �Dh��Di �Di��Dj �Dj��Dk �Dk��Dl �Dl��Dm �Dm��Dn �Dn��Do �Do��Dp �Dp��Dq �Dq��Dr �Dr��Ds �Ds��Dt �Dt��Du �Du��Dv �Dv��Dw �Dw��Dx �Dx��Dy �Dy��Dz �Dz��D{ �D{��D| �D|��D} �D}��D~ �D~��D �D��D� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RDRD��RD� RD�@RDÀRD��RD� RD�@RDĀRD��RD� RD�@RDŀRD��RD� RD�@RDƀRD��RD� RD�@RDǀRD��RD� RD�@RDȀRD��RD� RD�@RDɀRD��RD� RD�@RDʀRD��RD� RD�=DˀRD��RD� RD�@RD̀RD��RD� RD�@RD̀RD��RD� RD�@RD΀RD��RD� RD�@RDπRD��RD� RD�@RDЀRD��RD� RD�@RDрRD��RD� RD�@RDҀRD��RD� RD�@RDӀRD��RD� RD�@RDԀRD��RD� RD�@RDՀRD��RD� RD�@RDրRD��RD� RD�@RD׀RD��RD� RD�@RD؀RD��RD� RD�@RDـRD��RD� RD�@RDڀRD��RD� RD�@RDۀRD��RD� RD�@RD܀RD��RD� RD�@RD݀RD��RD� RD�@RDހRD��RD� RD�@RD߀RD��RD� RD�@RD��RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD��RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��R1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��A��A�$A�:A�IA�"4A�%�A�%FA�%zA�$@A�$�A�$A�&�A�)_A�,A�+kA�#�A�*eA�(XA�'�A�&LA�%zA�'�A�*�A�.IA�,qA�'RA�!bA�&�A�$�A�*�A�*�A���A��A���A���A��WA���A��2A��&A��A���A���A�רA�՛A���A�˒A���A̵�A̢�A�a�A�a|A���A�2�A�}"A�L�A���A�T,A�͟A�!-A�f2A��A�5A��MA��A���A�B�A�)�A�S&A�a�A��A��UA�R�A�"�A�A��A���A��iA�A�A��A��kA��bA�{�A�=A��A���A�P}A��FA�!�A��A��A|G�AtL�Al�*Ag��Ad�6A_�VA\^5AZ�AV�ARF�AO�1AM��AH�gAF�AD��AC�.AB<�A@'�A>��A;u%A8�4A6�TA5/A2�]A1��A1V�A0��A0A.��A,t�A*�A)xA'�A&u�A%�A%A$,�A"y>A"�A!�A!�1A!U2A Z�A��A -A�Ae,Aa�A�zA�ADgA\�A��A�LA��AqvAW�A��A33ASA/Ay�AیA$�A�:A�A��A�A'RA�A�.A��Am�AZ�A�A\�A=qA_pA�A�QA)_A?A�}A�bA&A�A	A��AoiA�Ay�A�Au%AL0AJ�A��AVA
x�A	��A	��A	~A	_A	~(AkQA��A`�A�hA�A�}A�DA'�A��AkQADgAOA�#A�_A_�A�	A�:AtTA$�A��A�jA�*Ae�A�A��A<6A2�A	A�A��A�A8A ��A ��A Y�@���@�N�@���@�@@�p;@�O@�@���@���@�q�@��@���@�A�@���@�@@�:*@�P�@�!@�Q�@��9@@�M@���@�b�@�|�@�c�@�C@�f@�`�@��@��9@�@���@�s�@�C�@扠@�t@�<6@��@�r�@��@㯸@��@��@�!-@��@�"h@�@@�@��A@�1'@��A@�m]@�S@޹�@ޫ6@�?�@ݰ�@�|�@ܣ@�Z@���@�#�@��X@�`�@��@� �@�7L@��B@�u%@�"h@�$t@ִ9@�{�@�x@�7L@�Q�@��@�K�@��8@ҭ�@�ff@�M@��@��@�L0@�@��#@�ϫ@Ϙ�@��@Υz@�l�@�_@�\)@� \@���@�L0@���@ˮ@�^�@ʰ!@�h
@��@�\)@�%@�Z@��)@Ǽ�@Ǉ�@��@ƆY@��W@ź^@�g�@�/@�;d@�8�@�Dg@�iD@�o�@�g�@�ں@��@�}�@�{�@���@��@��@��@�l"@�Z@�9X@�1@��@��K@�$t@�G@�N<@��I@�oi@�0U@��T@��@��L@�U2@��@��@��z@�#:@��3@��P@�:�@�Ft@�Dg@��v@��@�a|@�X�@��@��D@�Ft@���@��@�,=@��r@��@�iD@�H�@�<6@�/@� i@��@��A@�O�@��,@�`�@�[�@�Z@�Xy@�Z�@�Xy@�C-@�($@���@�S�@���@�[�@��.@��4@���@�q�@��o@���@���@���@�a@��@��f@��@��/@��h@���@�c @�3�@�j@���@��4@�xl@�.�@�!�@�	�@��@���@�%F@�ȴ@��D@�u�@�R�@�9X@���@�e�@���@�q�@��@���@�n/@�A�@�)_@�@��@���@��!@��\@� �@��0@��@�.I@��@�ѷ@���@���@�p;@��D@���@���@��:@�l�@�o@��`@���@�M�@��@�c�@�@@��)@�j@�!@��t@�Vm@���@�~(@�l"@�kQ@�B[@��Z@���@��~@�,�@���@�xl@�bN@�H@��@��@�ϫ@���@��f@�j@�P�@�7L@�-w@��s@�tT@�Ta@�_@���@��k@�#�@��O@�z�@�;�@��@��@��Q@��w@���@��:@�u�@�b�@�U�@�A @�V@���@���@�u�@� �@�ԕ@��F@���@�6z@�@��s@���@�bN@�5?@��@�ƨ@��[@��@�*0@���@���@��'@��D@�W�@�$@��&@��@�g�@�#�@� i@��p@��.@�tT@�-�@���@��w@��P@�g�@�;d@�$t@��@��@�ѷ@��}@��@�l�@�3�@��r@���@��M@�y�@�=@��@���@��e@���@�1'@���@�IR@�C@��@���@���@�m�@�E�@�%�@�e@��@���@��@��k@�\�@�9�@�/@�!-@��@��2@���@��@�m�@��@qv@/�@~�8@~�]@~�@}�@|�v@|��@{� @{1�@z�6@z�@y��@yV@x~(@x �@w�;@wv`@w1�@wS@v��@v�}@v��@vW�@v.�@u�H@t�@s�}@s��@sO@sA�@s�@r�@r�R@r� @r�\@ra|@q��@q^�@qIR@q/@q�@p�	@p�@pZ@o�Q@o{J@o]�@o@n�y@n��@n�@nO@m�#@m�@m�@m��@m-w@l�@k�@kn/@k�@j��@j$�@i�C@iA @h�@h�@gݘ@f҉@f�@e�-@eX@e-w@e%@d�D@d"h@c��@co�@b�'@bJ�@a�@a��@a&�@`�?@`u�@`$@_�W@_��@_x@^e@]��@]��@]�'@]�S@]s�@]q@]�@\֡@\��@\�@[��@[��@Z�<@Zn�@ZQ@Z-@Y��@Y�M@Y	l@XtT@X~@W��@W@VkQ@V!�@V@Vu@U��@U@U�@U�@T�I@T@S�@R�!@RR�@R$�@RJ@Q�D@Q��@Q��@Q*0@P��@P1'@P"h@O�@O�k@N��@N�@Mzx@M�@Lی@L�[@L��@L�D@L@K@O@K�@J��@Jv�@J;�@I�T@I�3@I�^@I�S@I0�@HPH@G�g@G�k@G�:@G~�@G�@Fff@F�@E��@E��@Ej@E \@E@D�K@DXy@D�@C�@B�c@B�@B��@B}V@BL0@A��@A�M@AIR@@��@@,=@?��@?S�@>��@>��@>u%@>+k@=��@==�@=!�@<�f@<��@<�@< �@;�g@;�Q@;ݘ@;�;@;�}@;=@:��@:�@:u@9�@9�@9��@9�~@9^�@8ѷ@8��@8��@8l"@8PH@8I�@8A�@8~@8	�@7��@7�g@7�$@7v`@7U�@7H�@74�@7
=@6�]@6ȴ@6�\@6v�@6R�@60U@6�@5�9@5s�@5N<@5�@4`�@4M@4 �@3��@3�0@3��@2��@1��@1q@0H@/�V@/U�@.�m@.B[@.J@-��@-+@,��@,��@,c�@+˒@+��@+iD@+9�@+!-@+�@*��@*ߤ@*�s@*�@*~�@*L0@*@)�H@)|@)2a@(��@(��@(�Y@(`�@(�@'�@'�Q@'��@'P�@'@O@'8@'�@&��@&h
@&{@%�@$��@$q@$6@$�@#��@#�K@#t�@#�@"�M@"҉@"�F@"�\@"u%@"Q@!�.@!��@!�T@!�T@!��@!��@!��@!rG@!*0@ �@ �@ ��@ u�@ ]d@ �@U�@
=@�c@�2@�F@i�@;�@@f�@A @�/@~(@K^@4n@�@��@e�@33@
=@��@��@z@d�@H�@5?@�@4@�@�z@��@f�@7L@�5@�@Q�@-�@�@�m@��@�q@�@�f@/�@�@
=@��@�R@xl@l�@6�@#:@�@�@�@��@�@rG@�@�@�9@��@�@I�@��@�@��@=@��@��@�r@�+@z@	@�j1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��A��A�$A�:A�IA�"4A�%�A�%FA�%zA�$@A�$�A�$A�&�A�)_A�,A�+kA�#�A�*eA�(XA�'�A�&LA�%zA�'�A�*�A�.IA�,qA�'RA�!bA�&�A�$�A�*�A�*�A���A��A���A���A��WA���A��2A��&A��A���A���A�רA�՛A���A�˒A���A̵�A̢�A�a�A�a|A���A�2�A�}"A�L�A���A�T,A�͟A�!-A�f2A��A�5A��MA��A���A�B�A�)�A�S&A�a�A��A��UA�R�A�"�A�A��A���A��iA�A�A��A��kA��bA�{�A�=A��A���A�P}A��FA�!�A��A��A|G�AtL�Al�*Ag��Ad�6A_�VA\^5AZ�AV�ARF�AO�1AM��AH�gAF�AD��AC�.AB<�A@'�A>��A;u%A8�4A6�TA5/A2�]A1��A1V�A0��A0A.��A,t�A*�A)xA'�A&u�A%�A%A$,�A"y>A"�A!�A!�1A!U2A Z�A��A -A�Ae,Aa�A�zA�ADgA\�A��A�LA��AqvAW�A��A33ASA/Ay�AیA$�A�:A�A��A�A'RA�A�.A��Am�AZ�A�A\�A=qA_pA�A�QA)_A?A�}A�bA&A�A	A��AoiA�Ay�A�Au%AL0AJ�A��AVA
x�A	��A	��A	~A	_A	~(AkQA��A`�A�hA�A�}A�DA'�A��AkQADgAOA�#A�_A_�A�	A�:AtTA$�A��A�jA�*Ae�A�A��A<6A2�A	A�A��A�A8A ��A ��A Y�@���@�N�@���@�@@�p;@�O@�@���@���@�q�@��@���@�A�@���@�@@�:*@�P�@�!@�Q�@��9@@�M@���@�b�@�|�@�c�@�C@�f@�`�@��@��9@�@���@�s�@�C�@扠@�t@�<6@��@�r�@��@㯸@��@��@�!-@��@�"h@�@@�@��A@�1'@��A@�m]@�S@޹�@ޫ6@�?�@ݰ�@�|�@ܣ@�Z@���@�#�@��X@�`�@��@� �@�7L@��B@�u%@�"h@�$t@ִ9@�{�@�x@�7L@�Q�@��@�K�@��8@ҭ�@�ff@�M@��@��@�L0@�@��#@�ϫ@Ϙ�@��@Υz@�l�@�_@�\)@� \@���@�L0@���@ˮ@�^�@ʰ!@�h
@��@�\)@�%@�Z@��)@Ǽ�@Ǉ�@��@ƆY@��W@ź^@�g�@�/@�;d@�8�@�Dg@�iD@�o�@�g�@�ں@��@�}�@�{�@���@��@��@��@�l"@�Z@�9X@�1@��@��K@�$t@�G@�N<@��I@�oi@�0U@��T@��@��L@�U2@��@��@��z@�#:@��3@��P@�:�@�Ft@�Dg@��v@��@�a|@�X�@��@��D@�Ft@���@��@�,=@��r@��@�iD@�H�@�<6@�/@� i@��@��A@�O�@��,@�`�@�[�@�Z@�Xy@�Z�@�Xy@�C-@�($@���@�S�@���@�[�@��.@��4@���@�q�@��o@���@���@���@�a@��@��f@��@��/@��h@���@�c @�3�@�j@���@��4@�xl@�.�@�!�@�	�@��@���@�%F@�ȴ@��D@�u�@�R�@�9X@���@�e�@���@�q�@��@���@�n/@�A�@�)_@�@��@���@��!@��\@� �@��0@��@�.I@��@�ѷ@���@���@�p;@��D@���@���@��:@�l�@�o@��`@���@�M�@��@�c�@�@@��)@�j@�!@��t@�Vm@���@�~(@�l"@�kQ@�B[@��Z@���@��~@�,�@���@�xl@�bN@�H@��@��@�ϫ@���@��f@�j@�P�@�7L@�-w@��s@�tT@�Ta@�_@���@��k@�#�@��O@�z�@�;�@��@��@��Q@��w@���@��:@�u�@�b�@�U�@�A @�V@���@���@�u�@� �@�ԕ@��F@���@�6z@�@��s@���@�bN@�5?@��@�ƨ@��[@��@�*0@���@���@��'@��D@�W�@�$@��&@��@�g�@�#�@� i@��p@��.@�tT@�-�@���@��w@��P@�g�@�;d@�$t@��@��@�ѷ@��}@��@�l�@�3�@��r@���@��M@�y�@�=@��@���@��e@���@�1'@���@�IR@�C@��@���@���@�m�@�E�@�%�@�e@��@���@��@��k@�\�@�9�@�/@�!-@��@��2@���@��@�m�@��@qv@/�@~�8@~�]@~�@}�@|�v@|��@{� @{1�@z�6@z�@y��@yV@x~(@x �@w�;@wv`@w1�@wS@v��@v�}@v��@vW�@v.�@u�H@t�@s�}@s��@sO@sA�@s�@r�@r�R@r� @r�\@ra|@q��@q^�@qIR@q/@q�@p�	@p�@pZ@o�Q@o{J@o]�@o@n�y@n��@n�@nO@m�#@m�@m�@m��@m-w@l�@k�@kn/@k�@j��@j$�@i�C@iA @h�@h�@gݘ@f҉@f�@e�-@eX@e-w@e%@d�D@d"h@c��@co�@b�'@bJ�@a�@a��@a&�@`�?@`u�@`$@_�W@_��@_x@^e@]��@]��@]�'@]�S@]s�@]q@]�@\֡@\��@\�@[��@[��@Z�<@Zn�@ZQ@Z-@Y��@Y�M@Y	l@XtT@X~@W��@W@VkQ@V!�@V@Vu@U��@U@U�@U�@T�I@T@S�@R�!@RR�@R$�@RJ@Q�D@Q��@Q��@Q*0@P��@P1'@P"h@O�@O�k@N��@N�@Mzx@M�@Lی@L�[@L��@L�D@L@K@O@K�@J��@Jv�@J;�@I�T@I�3@I�^@I�S@I0�@HPH@G�g@G�k@G�:@G~�@G�@Fff@F�@E��@E��@Ej@E \@E@D�K@DXy@D�@C�@B�c@B�@B��@B}V@BL0@A��@A�M@AIR@@��@@,=@?��@?S�@>��@>��@>u%@>+k@=��@==�@=!�@<�f@<��@<�@< �@;�g@;�Q@;ݘ@;�;@;�}@;=@:��@:�@:u@9�@9�@9��@9�~@9^�@8ѷ@8��@8��@8l"@8PH@8I�@8A�@8~@8	�@7��@7�g@7�$@7v`@7U�@7H�@74�@7
=@6�]@6ȴ@6�\@6v�@6R�@60U@6�@5�9@5s�@5N<@5�@4`�@4M@4 �@3��@3�0@3��@2��@1��@1q@0H@/�V@/U�@.�m@.B[@.J@-��@-+@,��@,��@,c�@+˒@+��@+iD@+9�@+!-@+�@*��@*ߤ@*�s@*�@*~�@*L0@*@)�H@)|@)2a@(��@(��@(�Y@(`�@(�@'�@'�Q@'��@'P�@'@O@'8@'�@&��@&h
@&{@%�@$��@$q@$6@$�@#��@#�K@#t�@#�@"�M@"҉@"�F@"�\@"u%@"Q@!�.@!��@!�T@!�T@!��@!��@!��@!rG@!*0@ �@ �@ ��@ u�@ ]d@ �@U�@
=@�c@�2@�F@i�@;�@@f�@A @�/@~(@K^@4n@�@��@e�@33@
=@��@��@z@d�@H�@5?@�@4@�@�z@��@f�@7L@�5@�@Q�@-�@�@�m@��@�q@�@�f@/�@�@
=@��@�R@xl@l�@6�@#:@�@�@�@��@�@rG@�@�@�9@��@�@I�@��@�@��@=@��@��@�r@�+@z@	@�j1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  Bm�Bm]Bm�Bm�Bm�BmwBm]BmwBm)BmCBl�Bl�Bl�Bl�BmCBmCBmBm)Bl�BmBl�Bl�Bm)Bm]BmCBm)Bl�Bl�Bl�Bl�Bl�Bl�BkBjBjeBjeBj0Bi�Bi�BiyBiDBh�Bh�Bh$Bh$Bg�Bg�BgBe�Bd@B`BB<�B<6Be,B��B�ZB	  B	�$B
/ B
ǔB
�yB
�FB
��B�B&B	7B�B4B�B�B+B*KB:B
�pB
��B
��B
�:B
{�B
r�B
gB
X�B
;�B
�B
B	��B	��B	�kB	�B	ںB	�\B	�B	��B	��B	g�B	\B	V9B	NVB	GEB	G�B	@�B	?B	4TB	$�B	WB	�B	�B	5�B	<B	4nB	?�B	88B	-B	&�B	#�B	"�B	�B	�B	#B	,B	B�8B��B�B�B�B��B�RB��B	
=B	NB	/B	(�B	:�B	8RB	:B	XB	\�B	Z�B	]dB	Z7B	bB	cB	\xB	U�B	OB	?HB	8lB	5ZB	5ZB	72B	9�B	>(B	D3B	LdB	h�B	u%B	z*B	�oB	��B	�rB	�XB	�DB	�B	��B	��B	�6B	�^B	��B	�.B	�B	��B	��B	ɺB	�FB	�B	��B	�(B	�B	��B	�"B	�<B	�pB	��B	�pB	�	B	�?B	�?B	��B	�EB	��B	�%B	�B	�B	ѷB	˒B	ǔB	�dB	�B	��B	��B	�OB	��B	�VB	߾B	ߤB	��B	��B	�-B	��B	�B	�TB	�nB	�B	�4B	�|B	�|B	�B	�B	�B	�B	��B	�B	�FB	��B	�zB	�B	��B	�B	�B	�$B	�mB	�zB	�B	��B	��B	��B	��B	�B	��B	�B	�B	��B	�B	��B	��B	�!B	�jB	�~B	�IB	�IB	�)B	ۦB	ۦB	߾B	�'B	��B	�7B	��B	�B	�9B	�1B	��B	��B	�WB	ںB	�B	ؓB	��B	רB	��B	ٴB	�B	�WB	��B	��B	��B	޸B	�B	�B	�IB	�B	��B	ܬB	��B	یB	ܬB	�~B	�5B	�pB	߾B	ߊB	�pB	߾B	�BB	��B	�-B	�B	�B	�B	�B	�tB	��B	�B	�B	�B	�B	� B	� B	��B	�4B	�B	�-B	�B	�B	߾B	߾B	߾B	ߤB	߾B	�BB	�BB	��B	�B	�-B	��B	�'B	��B	��B	�'B	�4B	�B	�nB	��B	�B	��B	�,B	�B	��B	�B	�,B	��B	��B	�B	�tB	�,B	�mB	�B	�B	��B	�'B	�B	�MB	�B	��B	�B	�WB	��B	�]B	��B	��B	��B	��B	�B	�B	��B	��B	��B	�B	��B	�B	��B	�B	�tB	�tB	��B	�B	��B	��B	�lB	��B	��B	��B	�*B	��B	�xB	�xB	�DB	�6B	�B	��B	�jB	��B	�B	�PB	�B	�B	��B	��B	��B	�PB	�6B	�B	�jB	�jB	��B	�"B	�qB	��B	��B	��B	��B	��B	��B	�VB	�6B	�xB	��B	�rB	��B	�DB	�B	��B	�B	�B	�B	��B	�qB	�qB	��B
  B
 �B
;B
oB
B
 OB
 �B
 iB
 �B
B
UB
UB
UB
�B
[B
�B
�B
�B
uB
�B
-B
�B
�B
{B
�B
9B
%B
B
�B
�B
�B
	B
	B
�B
	RB

�B
�B
JB
0B
~B
B
B
B
�B
jB
6B
B
�B
�B
<B
�B
�B
�B
�B
}B
hB
�B
�B
oB
oB
�B
�B
�B
�B
�B
�B
�B
�B
FB
�B
B
gB
�B
�B
�B
�B
�B
$B
?B
sB
�B
YB
B
yB
yB
�B
�B
�B
�B
QB
�B
�B
	B
�B
#B
#B
WB
WB
qB
qB
qB
�B
�B
�B
)B
)B
�B
B
IB
/B
�B
B
�B
�B
!B
VB
�B
�B
�B
 'B
 �B
!B
!-B
!-B
!|B
!�B
!�B
"4B
"NB
"�B
# B
#:B
#�B
#�B
#�B
$@B
$ZB
$�B
$�B
%FB
%`B
%�B
%`B
%`B
%zB
%�B
&LB
&2B
&�B
&�B
'8B
'�B
'mB
'�B
(>B
(�B
(>B
(XB
(�B
)�B
*�B
*�B
+B
+QB
+�B
,=B
,�B
,�B
,�B
,�B
,�B
,�B
,�B
-wB
-�B
-�B
-�B
-�B
-�B
-�B
-�B
.B
.�B
.�B
/5B
/�B
/�B
/�B
0B
0�B
0�B
1AB
1AB
1[B
1�B
2|B
2aB
3MB
3MB
3MB
3MB
3hB
3hB
3�B
3�B
3�B
3�B
3�B
3�B
5B
5%B
5ZB
5�B
5�B
5�B
5�B
5�B
5�B
5�B
6�B
6�B
6�B
6�B
6�B
6�B
7B
7fB
7fB
8B
8lB
8�B
9$B
9rB
9XB
:B
:�B
:�B
:�B
:xB
:DB
:xB
:�B
;�B
;B
;�B
<B
<PB
<�B
<�B
=�B
=�B
=�B
>�B
?.B
?cB
?�B
?�B
?�B
@4B
@OB
@iB
@�B
AoB
A�B
A�B
A�B
BAB
BAB
BuB
B�B
B�B
B�B
B�B
C�B
C�B
C�B
D3B
DB
D3B
D�B
DgB
D�B
D�B
EB
ESB
EmB
FB
F?B
F?B
FYB
FtB
F�B
GB
G_B
GzB
G�B
H1B
H�B
H�B
H�B
H�B
H�B
H�B
IB
IRB
IlB
I�B
J�B
J�B
J�B
KB
KB
KB
KB
KB
K^B
K�B
K�B
K�B
K�B
LB
L�B
MB
MjB
M�B
M�B
M�B
M�B
M�B
N<B
N�B
N�B
N�B
OB
O(B
OvB
OvB
O\B
O\B
O�B
P}B
P�B
P�B
P�B
P}B
P�B
QNB
QhB
QNB
Q�B
Q�B
Q�B
Q�B
Q�B
R:B
R:B
R�B
S[B
S[B
S[B
S[B
S[B
S�B
S�B
TB
T�B
T�B
T�B
U�B
U�B
U�B
VB
VB
V�B
V�B
V�B
W$B
W
B
WsB
W�B
XB
XB
W�B
W�B
W�B
XyB
XyB
X�B
YeB
YeB
YeB
YB
Y�B
Y�B
ZQB
Z7B
ZQB
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
[#B
[=B
[WB
[WB
[WB
[qB
[�B
[�B
[�B
[�B
\B
\B
\B
\)B
\�B
\�B
\�B
]IB
]IB
]/B
]/B
]IB
]B
]IB
]�B
]�B
^5B
^OB
^OB
^�B
^�B
^�B
_B
_�B
_�B
_�B
`BB
aHB
a�B
a�B
a�B
a�B
bB
b4B
b4B
bNB
bhB
b�B
b�B
cB
c B
cTB
cnB
dB
d&B
d&B
d@B
dtB
d�B
d�B
d�B
e,B
e,B
eB
eB
e`B
e�B
e�B
f�B
gRB
g�B
g�B
h
B
h$B
h$B
h�B
h�B
iB
i*B
i*B
i*B
i*B
iDB
i�B
i�B
i�B
i�B
i�B
i�B
i�B
i�B
j0B
jKB
j�B
j�B
j�B
j�B
j�B
kkB
k�B
k�B
k�B
l=B
lWB
lqB
mCB
m�B
mwB
m�B
m�B
m�B
m�B
m�B
m�B
n}B
n�B
n�B
o B
oiB
oiB
o�B
o�B
o�B
o�B
o�B
o�B
p!B
pUB
pUB
poB
p�B
qB
q[B
qvB
q�B
qvB
q�B
q�B
q�B
q�B
rGB
rGB
rGB
raB
r�B
r�B
r�B
s3B
s3B
shB
s�B
tB
tB
tB
tB
t�B
t�B
t�B
t�B
uB
u?B
u�B
u�B
u�B
v+B
v�B
v�B
v�B
v�B
v�B
wLB
wf1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  Bm�Bm]Bm�Bm�Bm�BmwBm]BmwBm)BmCBl�Bl�Bl�Bl�BmCBmCBmBm)Bl�BmBl�Bl�Bm)Bm]BmCBm)Bl�Bl�Bl�Bl�Bl�Bl�BkBjBjeBjeBj0Bi�Bi�BiyBiDBh�Bh�Bh$Bh$Bg�Bg�BgBe�Bd@B`BB<�B<6Be,B��B�ZB	  B	�$B
/ B
ǔB
�yB
�FB
��B�B&B	7B�B4B�B�B+B*KB:B
�pB
��B
��B
�:B
{�B
r�B
gB
X�B
;�B
�B
B	��B	��B	�kB	�B	ںB	�\B	�B	��B	��B	g�B	\B	V9B	NVB	GEB	G�B	@�B	?B	4TB	$�B	WB	�B	�B	5�B	<B	4nB	?�B	88B	-B	&�B	#�B	"�B	�B	�B	#B	,B	B�8B��B�B�B�B��B�RB��B	
=B	NB	/B	(�B	:�B	8RB	:B	XB	\�B	Z�B	]dB	Z7B	bB	cB	\xB	U�B	OB	?HB	8lB	5ZB	5ZB	72B	9�B	>(B	D3B	LdB	h�B	u%B	z*B	�oB	��B	�rB	�XB	�DB	�B	��B	��B	�6B	�^B	��B	�.B	�B	��B	��B	ɺB	�FB	�B	��B	�(B	�B	��B	�"B	�<B	�pB	��B	�pB	�	B	�?B	�?B	��B	�EB	��B	�%B	�B	�B	ѷB	˒B	ǔB	�dB	�B	��B	��B	�OB	��B	�VB	߾B	ߤB	��B	��B	�-B	��B	�B	�TB	�nB	�B	�4B	�|B	�|B	�B	�B	�B	�B	��B	�B	�FB	��B	�zB	�B	��B	�B	�B	�$B	�mB	�zB	�B	��B	��B	��B	��B	�B	��B	�B	�B	��B	�B	��B	��B	�!B	�jB	�~B	�IB	�IB	�)B	ۦB	ۦB	߾B	�'B	��B	�7B	��B	�B	�9B	�1B	��B	��B	�WB	ںB	�B	ؓB	��B	רB	��B	ٴB	�B	�WB	��B	��B	��B	޸B	�B	�B	�IB	�B	��B	ܬB	��B	یB	ܬB	�~B	�5B	�pB	߾B	ߊB	�pB	߾B	�BB	��B	�-B	�B	�B	�B	�B	�tB	��B	�B	�B	�B	�B	� B	� B	��B	�4B	�B	�-B	�B	�B	߾B	߾B	߾B	ߤB	߾B	�BB	�BB	��B	�B	�-B	��B	�'B	��B	��B	�'B	�4B	�B	�nB	��B	�B	��B	�,B	�B	��B	�B	�,B	��B	��B	�B	�tB	�,B	�mB	�B	�B	��B	�'B	�B	�MB	�B	��B	�B	�WB	��B	�]B	��B	��B	��B	��B	�B	�B	��B	��B	��B	�B	��B	�B	��B	�B	�tB	�tB	��B	�B	��B	��B	�lB	��B	��B	��B	�*B	��B	�xB	�xB	�DB	�6B	�B	��B	�jB	��B	�B	�PB	�B	�B	��B	��B	��B	�PB	�6B	�B	�jB	�jB	��B	�"B	�qB	��B	��B	��B	��B	��B	��B	�VB	�6B	�xB	��B	�rB	��B	�DB	�B	��B	�B	�B	�B	��B	�qB	�qB	��B
  B
 �B
;B
oB
B
 OB
 �B
 iB
 �B
B
UB
UB
UB
�B
[B
�B
�B
�B
uB
�B
-B
�B
�B
{B
�B
9B
%B
B
�B
�B
�B
	B
	B
�B
	RB

�B
�B
JB
0B
~B
B
B
B
�B
jB
6B
B
�B
�B
<B
�B
�B
�B
�B
}B
hB
�B
�B
oB
oB
�B
�B
�B
�B
�B
�B
�B
�B
FB
�B
B
gB
�B
�B
�B
�B
�B
$B
?B
sB
�B
YB
B
yB
yB
�B
�B
�B
�B
QB
�B
�B
	B
�B
#B
#B
WB
WB
qB
qB
qB
�B
�B
�B
)B
)B
�B
B
IB
/B
�B
B
�B
�B
!B
VB
�B
�B
�B
 'B
 �B
!B
!-B
!-B
!|B
!�B
!�B
"4B
"NB
"�B
# B
#:B
#�B
#�B
#�B
$@B
$ZB
$�B
$�B
%FB
%`B
%�B
%`B
%`B
%zB
%�B
&LB
&2B
&�B
&�B
'8B
'�B
'mB
'�B
(>B
(�B
(>B
(XB
(�B
)�B
*�B
*�B
+B
+QB
+�B
,=B
,�B
,�B
,�B
,�B
,�B
,�B
,�B
-wB
-�B
-�B
-�B
-�B
-�B
-�B
-�B
.B
.�B
.�B
/5B
/�B
/�B
/�B
0B
0�B
0�B
1AB
1AB
1[B
1�B
2|B
2aB
3MB
3MB
3MB
3MB
3hB
3hB
3�B
3�B
3�B
3�B
3�B
3�B
5B
5%B
5ZB
5�B
5�B
5�B
5�B
5�B
5�B
5�B
6�B
6�B
6�B
6�B
6�B
6�B
7B
7fB
7fB
8B
8lB
8�B
9$B
9rB
9XB
:B
:�B
:�B
:�B
:xB
:DB
:xB
:�B
;�B
;B
;�B
<B
<PB
<�B
<�B
=�B
=�B
=�B
>�B
?.B
?cB
?�B
?�B
?�B
@4B
@OB
@iB
@�B
AoB
A�B
A�B
A�B
BAB
BAB
BuB
B�B
B�B
B�B
B�B
C�B
C�B
C�B
D3B
DB
D3B
D�B
DgB
D�B
D�B
EB
ESB
EmB
FB
F?B
F?B
FYB
FtB
F�B
GB
G_B
GzB
G�B
H1B
H�B
H�B
H�B
H�B
H�B
H�B
IB
IRB
IlB
I�B
J�B
J�B
J�B
KB
KB
KB
KB
KB
K^B
K�B
K�B
K�B
K�B
LB
L�B
MB
MjB
M�B
M�B
M�B
M�B
M�B
N<B
N�B
N�B
N�B
OB
O(B
OvB
OvB
O\B
O\B
O�B
P}B
P�B
P�B
P�B
P}B
P�B
QNB
QhB
QNB
Q�B
Q�B
Q�B
Q�B
Q�B
R:B
R:B
R�B
S[B
S[B
S[B
S[B
S[B
S�B
S�B
TB
T�B
T�B
T�B
U�B
U�B
U�B
VB
VB
V�B
V�B
V�B
W$B
W
B
WsB
W�B
XB
XB
W�B
W�B
W�B
XyB
XyB
X�B
YeB
YeB
YeB
YB
Y�B
Y�B
ZQB
Z7B
ZQB
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
[#B
[=B
[WB
[WB
[WB
[qB
[�B
[�B
[�B
[�B
\B
\B
\B
\)B
\�B
\�B
\�B
]IB
]IB
]/B
]/B
]IB
]B
]IB
]�B
]�B
^5B
^OB
^OB
^�B
^�B
^�B
_B
_�B
_�B
_�B
`BB
aHB
a�B
a�B
a�B
a�B
bB
b4B
b4B
bNB
bhB
b�B
b�B
cB
c B
cTB
cnB
dB
d&B
d&B
d@B
dtB
d�B
d�B
d�B
e,B
e,B
eB
eB
e`B
e�B
e�B
f�B
gRB
g�B
g�B
h
B
h$B
h$B
h�B
h�B
iB
i*B
i*B
i*B
i*B
iDB
i�B
i�B
i�B
i�B
i�B
i�B
i�B
i�B
j0B
jKB
j�B
j�B
j�B
j�B
j�B
kkB
k�B
k�B
k�B
l=B
lWB
lqB
mCB
m�B
mwB
m�B
m�B
m�B
m�B
m�B
m�B
n}B
n�B
n�B
o B
oiB
oiB
o�B
o�B
o�B
o�B
o�B
o�B
p!B
pUB
pUB
poB
p�B
qB
q[B
qvB
q�B
qvB
q�B
q�B
q�B
q�B
rGB
rGB
rGB
raB
r�B
r�B
r�B
s3B
s3B
shB
s�B
tB
tB
tB
tB
t�B
t�B
t�B
t�B
uB
u?B
u�B
u�B
u�B
v+B
v�B
v�B
v�B
v�B
v�B
wLB
wf1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104953  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604175246  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604175246  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604175246                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605025253  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605025253  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610141505                      G�O�G�O�G�O�                