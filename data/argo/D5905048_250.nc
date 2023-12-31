CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-06-13T00:35:12Z creation;2018-06-13T00:35:17Z conversion to V3.1;2019-12-19T07:36:02Z update;     
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
resolution        =���   axis      Z        l  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     l  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \P   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     l  `,   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     l  st   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     l  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �(   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     l  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �p   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     l  �L   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     l  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     l  �$   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ې   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �    SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �    SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �    SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �    HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �    HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    � Argo profile    3.1 1.2 19500101000000  20180613003512  20200116231515  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0577_250                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @�i�9u1 1   @�i��s�@4bM���dP��>B[1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@���A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B ffB  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)�fD*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� DpfDp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D��3D��fD��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��R@��A (�A (�A@(�A`(�A�{A�{A�{A�{A�{A�{A�{A�{B p�B
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
=B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�8RB�B�B�B�B�B�B�B�B�B�B�C �C�C�C�C�C
�C�C�C�C�C�C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�C�HC�HC�HC�HC�HC�C�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HD  �D ��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D	 �D	��D
 �D
��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D  �D ��D! �D!��D" �D"��D# �D#��D$ �D$��D% �D%��D& �D&��D' �D'��D( �D(��D) �D)�
D* �D*��D+ �D+��D, �D,��D- �D-��D. �D.��D/ �D/��D0 �D0��D1 �D1��D2 �D2��D3 �D3��D4 �D4��D5 �D5��D6 �D6��D7 �D7��D8 �D8��D9 �D9��D: �D:��D; �D;��D< �D<��D= �D=��D> �D>��D? �D?��D@ �D@��DA �DA��DB �DB��DC �DC��DD �DD��DE �DE��DF �DF��DG �DG��DH �DH��DI �DI��DJ �DJ��DK �DK��DL �DL��DM �DM��DN �DN��DO �DO��DP �DP��DQ �DQ��DR �DR��DS �DS��DT �DT��DU �DU��DV �DV��DW �DW��DX �DX��DY �DY��DZ �DZ��D[ �D[��D\ �D\��D] �D]��D^ �D^��D_ �D_��D` �D`��Da �Da��Db �Db��Dc �Dc��Dd �Dd��De �De��Df �Df��Dg �Dg��Dh �Dh��Di �Di��Dj �Dj��Dk �Dk��Dl �Dl��Dm �Dm��Dn �Dn��Do �Do��Dp
Dp��Dq �Dq��Dr �Dr��Ds �Ds��Dt �Dt��Du �Du��Dv �Dv��Dw �Dw��Dx �Dx��Dy �Dy��Dz �Dz��D{ �D{��D| �D|��D} �D}��D~ �D~��D �D��D� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�=D��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD��D�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD��D�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RD��RD��RD� RD�@RDRD��RD� RD�@RDÀRD��RD� RD�@RDĀRD��RD� RD�@RDŀRD��RD� RD�@RDƀRD��RD� RD�@RDǀRD��RD� RD�@RDȀRD��RD� RD�@RDɀRD��RD� RD�@RDʀRD��RD� RD�@RDˀRD��RD� RD�@RD̀RD��RD� RD�@RD̀RD��RD� RD�@RD΀RD��RD� RD�@RDπRD��RD� RD�@RDЀRD��RD� RD�@RDрRD��RD� RD�@RDҀRD��RD� RD�@RDӀRD��RD� RD�@RDԀRD��RD� RD�@RDՀRD��RD� RD�@RDրRD��RD� RD�@RD׀RD��RD� RD�@RD؀RD��RD� RD�@RDـRD��RD� RD�@RDڀRD��RD� RD�@RDۀRD��RD� RD�@RD܀RD��RD� RD�@RD݀RD��RD� RD�@RDހRD��RD� RD�@RD߀RD��RD� RD�@RD��RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD�D� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD��RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD�RD��RD� RD�@RD��RD��RD� RD�@RD���D�ƸD��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�r�A�VA�=qA�5?A�/A�-A�"�A� �A��A�VA���A�t�A�G�A���A���A�+A�ȴAΓuA�ZA͏\A�AʸRAǸRA�hsAŮA�A�I�A��`A�33A�%A�?}A��TA��A�I�A��^A�+A��+A�ZA��A��RA�oA��HA�S�A��A�ffA�=qA�`BA�33A�\)A���A�;dA�bA��A�;dA���A�A�C�A���A�$�A�l�A��-A��A�
=A��A��HA��A���A��wA�M�A��A�I�A�1A�{A��-A���A�9XA�=qA�VA� �A�v�A���A�A��hA��A���A���A�A�"�A�r�A�VA�ȴA���A��PA�t�A�/A���A�C�A���A�"�A��7A��A�dZA���A��A��HA~�A~��A}�PAzz�Au��Ap��AooAn{AlZAj-AgK�Af{AdbAb5?A`ĜA_�A]%A[C�AX�9AWC�AV�AS�hAQ;dAN��AM&�AK��AJ��AJ1AI|�AH^5AG�AG��AGƨAGdZAFr�AE33AC�TAB��ABjA?�A=�-A<1'A:�\A81'A65?A4~�A3%A1
=A/�A.�jA-��A-|�A-dZA-C�A,�RA,1'A+`BA*n�A)�A&jA$�A#dZA"v�A!��A �jA��A/A��A1'AA&�A��A�DAA�A$�A��A��A�A�7AbNA�A�hA�A�\A�AoA9XA=qA
ĜA	\)A��AffA5?A�TA33A9XA�;AA\)An�A��A�A �/A 5?@��P@���@�G�@�b@���@���@�I�@��w@�v�@�O�@��
@�ff@�hs@���@�1'@�+@�~�@��#@��@�@ꗍ@�bN@��m@��@�ff@�D@�F@�7@�(�@�@ܼj@�33@ڇ+@�{@�`B@�9X@�ȴ@���@���@���@�5?@Ь@��
@���@��#@̓u@�;d@�5?@���@�G�@ȣ�@�(�@Ǯ@��@��@���@�o@�V@��@��T@�@�?}@��D@�9X@��@���@�$�@���@�/@���@�Q�@�1'@�dZ@�@��@��@��\@���@��9@��u@��@��#@�`B@��j@�  @��F@�\)@�ȴ@���@�@�hs@���@��@��9@�Z@���@�dZ@�C�@��!@�J@�7L@���@�bN@�1@��w@�dZ@��y@�^5@�J@�x�@��@�j@�9X@��w@��@�"�@��\@�J@�O�@��@��@�A�@��@���@���@�\)@�C�@��@��H@��@��R@�~�@�n�@�5?@�5?@��T@���@���@�x�@�?}@�7L@�/@�V@��/@��D@�9X@��@��m@�|�@�\)@�C�@���@���@�$�@��#@���@��7@�O�@�7L@�/@���@��u@�r�@�bN@�9X@���@���@��@�+@��@��@��!@���@�-@�J@�J@��#@��7@��@�O�@���@��@��@�A�@�b@��
@��@�|�@�@�ȴ@���@��R@��R@�ȴ@�n�@��@��^@�X@�?}@��@��@��9@�(�@��@���@�;d@�
=@�ȴ@���@��+@�n�@�V@�E�@��@��@���@�x�@�V@���@�z�@�j@�Q�@�(�@��m@�dZ@�
=@�@���@�~�@�-@��T@���@��7@�hs@�?}@�%@��@��/@��@��D@�bN@�9X@��
@��P@�l�@�S�@��@��@��!@��+@�ff@�=q@��@�@��-@��h@�X@�&�@���@��@�j@�Q�@�(�@���@��
@��
@�ƨ@���@�|�@�t�@�t�@�S�@�33@��@��R@�5?@�{@�@��@���@��^@���@�&�@���@��u@�j@�9X@�b@��;@���@�l�@�\)@�+@�"�@��H@��@���@���@�~�@�^5@�-@���@���@��@�p�@�p�@�G�@���@��@�r�@�Q�@�  @�w@�@;d@}�T@}�h@}?}@}V@|�@|�@{�@{C�@{"�@z�!@z�\@z^5@zJ@y��@yx�@x��@x�u@xb@w�P@w�@v��@v{@u�h@u?}@t�@t9X@s��@s�m@st�@s33@r�@r��@rJ@qhs@q&�@q�@q�@q%@p��@p��@o��@n�y@n5?@m�T@m�@mp�@mO�@m/@l�@lZ@l9X@k��@k��@k33@j�H@j��@j^5@i�#@i�7@i&�@h�9@hQ�@hb@g�;@gK�@f�@fȴ@f��@fv�@fV@f@ep�@e�@d��@d��@d�D@d1@c"�@b��@b�\@a��@aX@a%@`��@`�9@`�@`A�@_�@_�@_;d@^��@^��@]�@]�@]/@\�j@\j@[�m@[�@["�@Z�@Z��@Z�\@Y�#@Yx�@Y%@X�@XA�@X  @W��@W��@W��@WK�@W+@V�y@V�R@V�+@VV@U@U��@U�@U�@T��@T�@TI�@S�F@SS�@R��@R�\@Rn�@R�@Q�@Q��@QX@P��@P�u@PbN@P �@O��@O\)@N��@N��@N{@N@M�T@M��@M�h@L��@L�/@L�j@L�@L��@Lj@L1@K�F@Ko@J�H@J��@Jn�@J^5@J-@I��@Ihs@I�@H�9@H1'@G�@G��@G\)@F�y@F�R@F�+@F5?@E�@E@E��@Ep�@EO�@E�@D�j@Dz�@D9X@C�
@C��@CdZ@Co@B�@B�!@B=q@A�@A��@A��@Ahs@A&�@@�9@@r�@@Q�@@b@?�;@?�@?;d@?
=@>ȴ@>v�@>5?@=�T@=��@=�h@=O�@<�/@<��@<�D@<�D@<Z@;��@;�
@;t�@;"�@;@:�@:�H@:�!@:-@:�@9��@9��@9x�@97L@8�`@8�`@8�u@8  @7��@7|�@7+@6�y@6ȴ@6��@6�+@6�+@6v�@6ff@6E�@6V@6V@6E�@5@5O�@4�@4�j@4�@4��@4�D@4Z@3�m@3��@3t�@3dZ@3C�@3@2�\@2=q@2�@1��@1�#@1��@1�7@1&�@0��@0�u@0bN@/�@/�w@/�P@/;d@.ȴ@.{@.@-�@-�T@-�T@-��@-?}@,�j@,�j@,Z@+��@+�@+33@+o@+@*��@*~�@*^5@*-@*J@)��@)�@)�@)�@)��@)x�@)hs@)G�@)�@(�`@(�u@(�@(r�@(A�@( �@(  @'��@'��@'|�@'\)@'+@&��@&�@&�R@&��@&��@&�+@&v�@&V@&5?@%�@%�-@%p�@$��@$��@$�@$j@$�@#ƨ@#�@#t�@#C�@"�H@"�\@"~�@"~�@"^5@"-@!�@!�^@!X@ �`@ ��@ A�@ b@�;@��@�P@�@��@�+@v�@ff@ff@5?@@�h@`B@?}@/@��@�@�j@��@j@I�@(�@��@�
@t�@dZ@33@�H@��@��@n�@M�@��@�^@��@�7@hs@hs@&�@��@��@��@��@�u@r�@1'@��@�P@;d@�y@��@ff@5?@�T@�-@p�@`B@`B@O�@�@�@��@z�@9X@1@1@�m@�@33@33@33@"�@o@o@o@@�@�@�H@��@��@��@n�@-@�@�#@�7@hs@X@7L@&�@&�@�@%@�`@Ĝ@�9@��@�@r�@r�@Q�@ �@b@�;@��@|�@l�@\)@;d@+@+111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�r�A�VA�=qA�5?A�/A�-A�"�A� �A��A�VA���A�t�A�G�A���A���A�+A�ȴAΓuA�ZA͏\A�AʸRAǸRA�hsAŮA�A�I�A��`A�33A�%A�?}A��TA��A�I�A��^A�+A��+A�ZA��A��RA�oA��HA�S�A��A�ffA�=qA�`BA�33A�\)A���A�;dA�bA��A�;dA���A�A�C�A���A�$�A�l�A��-A��A�
=A��A��HA��A���A��wA�M�A��A�I�A�1A�{A��-A���A�9XA�=qA�VA� �A�v�A���A�A��hA��A���A���A�A�"�A�r�A�VA�ȴA���A��PA�t�A�/A���A�C�A���A�"�A��7A��A�dZA���A��A��HA~�A~��A}�PAzz�Au��Ap��AooAn{AlZAj-AgK�Af{AdbAb5?A`ĜA_�A]%A[C�AX�9AWC�AV�AS�hAQ;dAN��AM&�AK��AJ��AJ1AI|�AH^5AG�AG��AGƨAGdZAFr�AE33AC�TAB��ABjA?�A=�-A<1'A:�\A81'A65?A4~�A3%A1
=A/�A.�jA-��A-|�A-dZA-C�A,�RA,1'A+`BA*n�A)�A&jA$�A#dZA"v�A!��A �jA��A/A��A1'AA&�A��A�DAA�A$�A��A��A�A�7AbNA�A�hA�A�\A�AoA9XA=qA
ĜA	\)A��AffA5?A�TA33A9XA�;AA\)An�A��A�A �/A 5?@��P@���@�G�@�b@���@���@�I�@��w@�v�@�O�@��
@�ff@�hs@���@�1'@�+@�~�@��#@��@�@ꗍ@�bN@��m@��@�ff@�D@�F@�7@�(�@�@ܼj@�33@ڇ+@�{@�`B@�9X@�ȴ@���@���@���@�5?@Ь@��
@���@��#@̓u@�;d@�5?@���@�G�@ȣ�@�(�@Ǯ@��@��@���@�o@�V@��@��T@�@�?}@��D@�9X@��@���@�$�@���@�/@���@�Q�@�1'@�dZ@�@��@��@��\@���@��9@��u@��@��#@�`B@��j@�  @��F@�\)@�ȴ@���@�@�hs@���@��@��9@�Z@���@�dZ@�C�@��!@�J@�7L@���@�bN@�1@��w@�dZ@��y@�^5@�J@�x�@��@�j@�9X@��w@��@�"�@��\@�J@�O�@��@��@�A�@��@���@���@�\)@�C�@��@��H@��@��R@�~�@�n�@�5?@�5?@��T@���@���@�x�@�?}@�7L@�/@�V@��/@��D@�9X@��@��m@�|�@�\)@�C�@���@���@�$�@��#@���@��7@�O�@�7L@�/@���@��u@�r�@�bN@�9X@���@���@��@�+@��@��@��!@���@�-@�J@�J@��#@��7@��@�O�@���@��@��@�A�@�b@��
@��@�|�@�@�ȴ@���@��R@��R@�ȴ@�n�@��@��^@�X@�?}@��@��@��9@�(�@��@���@�;d@�
=@�ȴ@���@��+@�n�@�V@�E�@��@��@���@�x�@�V@���@�z�@�j@�Q�@�(�@��m@�dZ@�
=@�@���@�~�@�-@��T@���@��7@�hs@�?}@�%@��@��/@��@��D@�bN@�9X@��
@��P@�l�@�S�@��@��@��!@��+@�ff@�=q@��@�@��-@��h@�X@�&�@���@��@�j@�Q�@�(�@���@��
@��
@�ƨ@���@�|�@�t�@�t�@�S�@�33@��@��R@�5?@�{@�@��@���@��^@���@�&�@���@��u@�j@�9X@�b@��;@���@�l�@�\)@�+@�"�@��H@��@���@���@�~�@�^5@�-@���@���@��@�p�@�p�@�G�@���@��@�r�@�Q�@�  @�w@�@;d@}�T@}�h@}?}@}V@|�@|�@{�@{C�@{"�@z�!@z�\@z^5@zJ@y��@yx�@x��@x�u@xb@w�P@w�@v��@v{@u�h@u?}@t�@t9X@s��@s�m@st�@s33@r�@r��@rJ@qhs@q&�@q�@q�@q%@p��@p��@o��@n�y@n5?@m�T@m�@mp�@mO�@m/@l�@lZ@l9X@k��@k��@k33@j�H@j��@j^5@i�#@i�7@i&�@h�9@hQ�@hb@g�;@gK�@f�@fȴ@f��@fv�@fV@f@ep�@e�@d��@d��@d�D@d1@c"�@b��@b�\@a��@aX@a%@`��@`�9@`�@`A�@_�@_�@_;d@^��@^��@]�@]�@]/@\�j@\j@[�m@[�@["�@Z�@Z��@Z�\@Y�#@Yx�@Y%@X�@XA�@X  @W��@W��@W��@WK�@W+@V�y@V�R@V�+@VV@U@U��@U�@U�@T��@T�@TI�@S�F@SS�@R��@R�\@Rn�@R�@Q�@Q��@QX@P��@P�u@PbN@P �@O��@O\)@N��@N��@N{@N@M�T@M��@M�h@L��@L�/@L�j@L�@L��@Lj@L1@K�F@Ko@J�H@J��@Jn�@J^5@J-@I��@Ihs@I�@H�9@H1'@G�@G��@G\)@F�y@F�R@F�+@F5?@E�@E@E��@Ep�@EO�@E�@D�j@Dz�@D9X@C�
@C��@CdZ@Co@B�@B�!@B=q@A�@A��@A��@Ahs@A&�@@�9@@r�@@Q�@@b@?�;@?�@?;d@?
=@>ȴ@>v�@>5?@=�T@=��@=�h@=O�@<�/@<��@<�D@<�D@<Z@;��@;�
@;t�@;"�@;@:�@:�H@:�!@:-@:�@9��@9��@9x�@97L@8�`@8�`@8�u@8  @7��@7|�@7+@6�y@6ȴ@6��@6�+@6�+@6v�@6ff@6E�@6V@6V@6E�@5@5O�@4�@4�j@4�@4��@4�D@4Z@3�m@3��@3t�@3dZ@3C�@3@2�\@2=q@2�@1��@1�#@1��@1�7@1&�@0��@0�u@0bN@/�@/�w@/�P@/;d@.ȴ@.{@.@-�@-�T@-�T@-��@-?}@,�j@,�j@,Z@+��@+�@+33@+o@+@*��@*~�@*^5@*-@*J@)��@)�@)�@)�@)��@)x�@)hs@)G�@)�@(�`@(�u@(�@(r�@(A�@( �@(  @'��@'��@'|�@'\)@'+@&��@&�@&�R@&��@&��@&�+@&v�@&V@&5?@%�@%�-@%p�@$��@$��@$�@$j@$�@#ƨ@#�@#t�@#C�@"�H@"�\@"~�@"~�@"^5@"-@!�@!�^@!X@ �`@ ��@ A�@ b@�;@��@�P@�@��@�+@v�@ff@ff@5?@@�h@`B@?}@/@��@�@�j@��@j@I�@(�@��@�
@t�@dZ@33@�H@��@��@n�@M�@��@�^@��@�7@hs@hs@&�@��@��@��@��@�u@r�@1'@��@�P@;d@�y@��@ff@5?@�T@�-@p�@`B@`B@O�@�@�@��@z�@9X@1@1@�m@�@33@33@33@"�@o@o@o@@�@�@�H@��@��@��@n�@-@�@�#@�7@hs@X@7L@&�@&�@�@%@�`@Ĝ@�9@��@�@r�@r�@Q�@ �@b@�;@��@|�@l�@\)@;d@+@+111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�
B	�sB
B
JB
oB
PB
B
%B
\B
�B
=qB
�hB
ǮBP�B�B��B�?B�
B�;B��B�B�HB��BB\B.B6FB:^Be`Bk�BcTBZBdZBiyBiyBgmBq�B�B|�B|�B�B�Bn�Bo�Br�BbNBm�Bk�BffB`BBK�BZB^5BgmBffB\)BN�B^5BW
BVBQ�B>wB2-B�B{B��B�/B�;B��BǮB�B�VB�VB� BaHBI�B0!B�B�B
��B
�BB
��B
��B
�B
�B
�;B
B
ŢB
��B
�FB
��B
�\B
~�B
hsB
M�B
;dB
C�B
5?B
hB	�B	ɺB	ȴB	ǮB	�RB	��B	�DB	�JB	{�B	n�B	hsB	ZB	K�B	@�B	2-B	,B	&�B	oB	B��B	B��B	B	B	  B��B��B	  B	  B��B�B�B�mB�TB�TB��BB��B��B�B��B��B��B��B��B��B��B��B��B��B��B�uB�VB�B{�Bk�By�Bs�Bx�Bz�Br�Br�Bx�Bw�Bu�Bt�Bn�BffBjBp�Bq�BjB_;BT�BZB[#BW
BYB]/BcTB_;BYB\)BN�BVBT�B_;BaHBcTBaHB]/B\)BdZBffBbNB]/BXB]/BgmBgmBiyBgmBdZBdZBbNBffBjBjBhsBgmBgmBgmBm�Bp�Bo�Bm�Bp�Bq�Bm�Bl�Bo�BjBs�Bw�Bq�Bk�Bu�Bn�Bs�Bo�Bw�Bv�B|�B|�Bz�By�Bx�B}�B{�B}�Bz�By�B�B�B�B�B�B�DB�hB�oB�uB��B��B�{B�uB��B��B��B��B�B��B��B��B�B�B�B�-B�?B�LB�XB�^B�wB�qBBƨBǮBǮBÖBƨB��B�B��B�BB�HB�TB�B�B�B�B��B��B��B	B	B	B	B	+B	1B	+B	
=B	VB	�B	�B	�B	 �B	!�B	#�B	&�B	+B	,B	/B	6FB	:^B	;dB	@�B	A�B	C�B	F�B	M�B	S�B	XB	\)B	^5B	_;B	bNB	dZB	iyB	jB	k�B	n�B	n�B	o�B	r�B	r�B	s�B	s�B	v�B	y�B	y�B	y�B	}�B	}�B	|�B	}�B	~�B	�B	�%B	�%B	�+B	�DB	�JB	�JB	�JB	�DB	�VB	�hB	�uB	�uB	��B	��B	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�-B	�-B	�3B	�9B	�FB	�FB	�RB	�dB	�}B	��B	��B	�}B	�qB	�jB	ÖB	ÖB	ŢB	ŢB	ƨB	ĜB	ŢB	ɺB	��B	��B	��B	��B	��B	�B	�
B	�
B	�B	�B	�B	�B	�#B	�B	�B	�/B	�;B	�;B	�;B	�;B	�5B	�HB	�TB	�TB	�NB	�ZB	�`B	�fB	�mB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B

=B

=B
JB
DB
JB
JB
JB
JB
JB
JB
JB
VB
VB
\B
\B
VB
JB
VB
PB
VB
PB
\B
bB
VB
PB
oB
uB
{B
uB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
"�B
"�B
!�B
!�B
!�B
!�B
"�B
"�B
%�B
&�B
&�B
&�B
%�B
$�B
"�B
#�B
%�B
'�B
'�B
(�B
(�B
(�B
'�B
(�B
)�B
(�B
(�B
(�B
)�B
)�B
)�B
(�B
)�B
+B
)�B
+B
+B
,B
+B
,B
-B
-B
,B
-B
,B
+B
,B
,B
.B
-B
,B
+B
.B
.B
-B
.B
/B
0!B
0!B
0!B
0!B
0!B
0!B
/B
0!B
0!B
0!B
1'B
2-B
2-B
33B
2-B
33B
49B
49B
49B
49B
33B
49B
5?B
5?B
7LB
7LB
8RB
8RB
7LB
7LB
8RB
8RB
8RB
8RB
8RB
8RB
9XB
9XB
9XB
9XB
:^B
9XB
9XB
:^B
:^B
;dB
<jB
;dB
<jB
<jB
;dB
;dB
<jB
=qB
=qB
<jB
<jB
=qB
=qB
=qB
?}B
?}B
?}B
?}B
>wB
@�B
@�B
@�B
@�B
@�B
?}B
?}B
@�B
A�B
B�B
A�B
B�B
B�B
A�B
B�B
B�B
A�B
A�B
B�B
C�B
B�B
B�B
C�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
D�B
E�B
E�B
D�B
E�B
E�B
E�B
F�B
E�B
E�B
E�B
F�B
F�B
F�B
E�B
E�B
F�B
F�B
F�B
G�B
G�B
F�B
G�B
G�B
G�B
H�B
H�B
H�B
I�B
H�B
H�B
I�B
J�B
J�B
J�B
I�B
J�B
J�B
J�B
K�B
K�B
L�B
K�B
J�B
L�B
L�B
L�B
K�B
K�B
L�B
M�B
K�B
K�B
L�B
M�B
M�B
N�B
N�B
O�B
O�B
P�B
P�B
P�B
P�B
P�B
Q�B
Q�B
P�B
P�B
Q�B
R�B
S�B
S�B
S�B
R�B
R�B
S�B
T�B
T�B
S�B
S�B
S�B
T�B
VB
VB
VB
VB
VB
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
W
B
W
B
XB
ZB
ZB
ZB
ZB
YB
YB
YB
[#B
ZB
ZB
[#B
[#B
\)B
\)B
[#B
\)B
\)B
\)B
\)B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
_;B
_;B
_;B
_;B
_;B
`BB
`BB
`BB
`BB
`BB
`BB
`BB
_;B
_;B
_;B
_;B
`BB
aHB
`BB
`BB
aHB
bNB
cTB
bNB
bNB
bNB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
dZB
dZB
e`B
e`B
ffB
ffB
e`B
e`B
gmB
gmB
gmB
gmB
gmB
ffB
gmB
gmB
gmB
hsB
hsB
hsB
hsB
hsB
hsB
iyB
iyB
iyB
iyB
iyB
jB
jB
jB
jB
k�B
jB
k�B
jB
k�B
l�B
l�B
l�B
l�B
k�B
l�B
m�B
l�B
l�B
m�B
l�B
l�B
l�B
l�B
l�B
m�B
n�B
n�B
n�B
n�B
o�B
o�B
p�B
p�B
o�B
o�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
p�B
q�B
r�B
r�B
r�B
r�B
s�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
v�B
v�B
v�B
v�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B	�NB	�&B	�B	�B	�B	�B	�B	�B	�SB	��B	�DB
aB
�B
B
pB
�B
�B
}B
B
AUB
�aB
��BR�B��B�-B��B��B�B�zB�nB�B�wBB�B/5B7�B<�Bf2Bl=Bd�B\xBe�Bj�Bj�Bi�BsMB��B~�B~wB��B�BqABp�Bs�BdZBo�BnIBh�Bc�BOB[�B_�Bg�BgB^BQ4B^�BX+BWYBS�BAUB4�B �B�B��B�B��B� B��B� B��B��B�'Bd�BN"B4TB �B�BGB
�B
�B
�B
�TB
��B
�4B
��B
�+B
��B
��B
�8B
�TB
�oB
k�B
P�B
>(B
DMB
7LB
�B	�B	��B	��B	�7B	��B	��B	��B	�B	~wB	p�B	jeB	\xB	NpB	B�B	5B	-�B	(�B	�B	1B	�B	�B��B	AB	�B	 �B�0B�}B	 OB	 OB��B�-B�;B�B��B�tB�B�SBðB��B�;B��B�
B��B�B�B�B��B��B��B�B�eB�aB��B��B~(Bn�B{�Bu�Bz*B|Bt9BtBy�Bx�BvzBu�Bo�BhXBk�BqBrBk�BaBWsB[�B\�BX�BZ�B^5Bc�B`BBZ�B]�BQ�BW�BV�B`Ba�Bc�Ba�B^OB]dBd�Bf�Bc:B^�BZB_Bh$BhXBj0Bh$BezBe`Bc�BgBkBkBiDBhXBhsBhsBnIBq'Bp;BncBqABrGBn}BmwBpoBk�BtBxBr�Bl�Bv�Bp!Bt�Bq'Bx�Bw�B}qB}qB{�Bz�By�B~wB|�B~�B|Bz�B��B��B��B�B�B��B��B��B��B��B�B�MB��B�B��B�TB�>B�B�0B�yB�yB�]B�wB��B��B��B��B��B��B��B��B��B��B��B�B�MB�_B�<B��B�B��B��B��B��B��B�B�B�FB�>B�<B	;B	UB	oB	�B	_B	fB	�B	
�B	�B	�B	�B	B	!B	!�B	$@B	'RB	+QB	,�B	/�B	6zB	:�B	;�B	@�B	A�B	DB	GB	NVB	TFB	X_B	\]B	^jB	_�B	b�B	d�B	i�B	j�B	k�B	n�B	n�B	o�B	r�B	r�B	s�B	tB	v�B	y�B	y�B	zB	}�B	}�B	}"B	~(B	HB	�GB	�?B	�YB	�zB	�^B	�dB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�B	��B	�B	�B	��B	�6B	�6B	�)B	�CB	�]B	�OB	�-B	�aB	�hB	�nB	�`B	�`B	��B	��B	�}B	��B	��B	��B	��B	��B	ðB	��B	żB	��B	��B	�B	�B	��B	�B	�"B	�B	� B	�B	�B	�$B	�$B	�+B	�1B	�EB	�1B	�=B	�eB	�eB	�IB	�VB	�VB	�pB	�pB	ޞB	�|B	�nB	�B	�B	�B	�B	�B	�mB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	�B	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	�B	��B	�B	��B	�.B	�B	�B	�<B	�<B
 B
 B
'B
'B
'B
AB
oB
GB
3B
9B
SB
SB
YB
?B
EB

XB

=B
dB
xB
dB
dB
dB
dB
dB
~B
~B
pB
pB
vB
vB
�B
�B
pB
�B
pB
�B
vB
bB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
"�B
"�B
!�B
!�B
!�B
!�B
#B
#B
%�B
&�B
&�B
&�B
%�B
%B
#:B
$&B
&B
(
B
(
B
(�B
)B
)B
($B
)B
)�B
)B
)*B
)B
*B
*B
*B
)*B
*B
+B
*0B
+B
+B
,"B
+QB
,"B
-B
-)B
,"B
-)B
,=B
+6B
,"B
,=B
-�B
-B
,=B
+QB
./B
./B
-CB
.cB
/5B
0!B
0!B
0;B
0;B
0!B
0;B
/OB
0;B
0UB
0oB
1[B
2GB
2aB
3MB
2aB
3hB
4TB
4TB
4TB
4TB
3hB
4nB
5tB
5tB
7fB
7fB
8RB
88B
7�B
7�B
8lB
8lB
8lB
8lB
8lB
8�B
9rB
9rB
9XB
9rB
:xB
9�B
9�B
:xB
:�B
;dB
<�B
;dB
<�B
<�B
;�B
;B
<jB
=qB
=qB
<�B
<�B
=�B
=�B
=�B
?}B
?}B
?�B
?�B
>�B
@�B
@�B
@�B
@�B
@�B
?�B
?�B
@�B
A�B
B�B
A�B
B�B
B�B
A�B
B�B
B�B
A�B
A�B
B�B
C�B
B�B
B�B
C�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
D�B
E�B
E�B
D�B
E�B
E�B
E�B
F�B
E�B
E�B
E�B
F�B
F�B
F�B
E�B
E�B
F�B
F�B
F�B
G�B
G�B
F�B
G�B
G�B
G�B
H�B
H�B
H�B
I�B
H�B
H�B
I�B
J�B
J�B
J�B
I�B
J�B
J�B
J�B
K�B
K�B
L�B
K�B
J�B
L�B
L�B
L�B
K�B
K�B
L�B
M�B
K�B
K�B
L�B
M�B
M�B
N�B
N�B
O�B
O�B
P�B
P�B
P�B
P�B
P�B
Q�B
RB
QB
QB
RB
SB
S�B
S�B
S�B
SB
S&B
TB
UB
T�B
TB
TB
T,B
UB
V9B
VB
VB
VB
VB
VB
VB
W$B
W$B
W?B
W$B
X+B
W?B
WYB
XEB
ZB
ZB
ZB
ZB
Y1B
YKB
YKB
[#B
ZQB
ZQB
[WB
[=B
\CB
\)B
[=B
\CB
\)B
\CB
\CB
]/B
]/B
]/B
]/B
]/B
]IB
]/B
]IB
]IB
]IB
]IB
^5B
^OB
^5B
^5B
^OB
^OB
^OB
_VB
_VB
_;B
_;B
_;B
`\B
`BB
`\B
`BB
`'B
`\B
`\B
_VB
_VB
_;B
_pB
`\B
abB
`\B
`\B
a|B
bhB
cTB
bhB
bhB
bhB
cTB
cTB
cnB
cnB
c�B
cnB
c�B
cnB
dtB
dtB
ezB
ezB
f�B
ffB
ezB
ezB
gmB
gmB
gmB
gRB
g�B
f�B
g�B
g�B
gmB
hXB
h�B
h�B
h�B
h�B
h�B
i�B
i�B
i�B
i�B
i�B
jB
j�B
j�B
j�B
k�B
j�B
k�B
j�B
k�B
l�B
l�B
l�B
l�B
k�B
l�B
m�B
l�B
l�B
m�B
l�B
l�B
l�B
l�B
l�B
m�B
n�B
n�B
n�B
n�B
o�B
o�B
p�B
p�B
o�B
o�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
p�B
q�B
r�B
r�B
r�B
r�B
s�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
v�B
v�B
v�B
v�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.01(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201806170037152018061700371520180617003715201806221331572018062213315720180622133157201806180024452018061800244520180618002445  JA  ARFMdecpA19c                                                                20180613093503  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180613003512  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180613003515  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180613003515  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180613003516  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180613003516  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180613003516  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180613003516  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180613003517  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180613003517                      G�O�G�O�G�O�                JA  ARUP                                                                        20180613005509                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180613153348  CV  JULD            G�O�G�O�F�O�                JM  ARCAJMQC2.0                                                                 20180616153715  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180616153715  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180617152445  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622043157  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116231515                      G�O�G�O�G�O�                