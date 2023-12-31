CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-03-30T12:00:30Z creation      
references        (http://www.argodatamgt.org/Documentation   comment           user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7(   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7h   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8,   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�E�vQ�   
_FillValue        A.�~       axis      T           80   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    88   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�E�vQ�   
_FillValue        A.�~            8<   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8D   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8L   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8T   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8X   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8`   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9`   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9d   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9h   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9l   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  A\   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  CX   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  KD   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M@   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  U,   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  g    TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  x�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  z�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �    HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �P   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �`   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �d   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �t   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �x   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �|   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��        ��Argo profile    3.1 1.2 19500101000000  20200330120030  20230721230926  4901659 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               �A   AO  5382                            2C  D   NAVIS_A                         0371                            082713                          863 @��`^�1   @�/hU�@:�\(��c�x���1   GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                      �A   A   A   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\y�D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}�3D}��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @���@�@�A�HA>�HA^�HA~�HA�p�A�p�A�p�A�p�A�p�A�p�A�p�A�p�B�RB�RB�RB�RB'�RB/�RB7�RB?�RBG�RBO�RBW�RB_�RBg�RBo�RBw�RB�RB��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B�\B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)C�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��=C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
D {�D ��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D	{�D	��D
{�D
��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D {�D ��D!{�D!��D"{�D"��D#{�D#��D${�D$��D%{�D%��D&{�D&��D'{�D'��D({�D(��D){�D)��D*{�D*��D+{�D+��D,{�D,��D-{�D-��D.{�D.��D/{�D/��D0{�D0��D1{�D1��D2{�D2��D3{�D3��D4{�D4��D5{�D5��D6{�D6��D7{�D7��D8{�D8��D9{�D9��D:{�D:��D;{�D;��D<{�D<��D={�D=��D>{�D>��D?{�D?��D@{�D@��DA{�DA��DB{�DB��DC{�DC��DD{�DD��DE{�DE��DF{�DF��DG{�DG��DH{�DH��DI{�DI��DJ{�DJ��DK{�DK��DL{�DL��DM{�DM��DN{�DN��DO{�DO��DP{�DP��DQ{�DQ��DR{�DR��DS{�DS��DT{�DT��DU{�DU��DV{�DV��DW{�DW��DX{�DX��DY{�DY��DZ{�DZ��D[{�D[��D\uD\��D]{�D]��D^{�D^��D_{�D_��D`{�D`��Da{�Da��Db{�Db��Dc{�Dc��Dd{�Dd��De{�De��Df{�Df��Dg{�Dg��Dh{�Dh��Di{�Di��Dj{�Dj��Dk{�Dk��Dl{�Dl��Dm{�Dm��Dn{�Dn��Do{�Do��Dp{�Dp��Dq{�Dq��Dr{�Dr��Ds{�Ds��Dt{�Dt��Du{�Du��Dv{�Dv��Dw{�Dw��Dx{�Dx��Dy{�Dy��Dz{�Dz��D{{�D{��D|{�D|��D}��D}�R111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A���A���A���A���A���A���A���A���A��TA��;A��TA��A���A��9A�z�A���A��A�z�A�A���A�n�A�"�A���A�  A���A�hsA�(�A��A�ƨA�Q�A���A��
A��RA���A��uA�M�A�+A�JA��wA�l�A�&�A��mA��RA�1'A���A�VA���A��yA��HA���A���A�/A���A�n�A�9XA�ƨA���A�Q�A��#A��A��A�&�A�+A��HA���A�|�A�?}A�r�A�$�A�XA���A��7A���A�hsA�A��+A��A�-A���A�1'A��\A�A��DA���A�{A�VA���A�z�A�;dA���A��`A��^A���A�G�A�p�A�\)A�(�A��\A�jA��A�$�A~��A{�TAy?}Av^5At�jAs|�Ar-Ao�hAn��Am�mAlQ�Aj�Ah�`Ah��Ahz�Ag�7AehsAd�uAc+A_+A]O�AZ�yAWAU�TAS��AQ?}AOO�AM�AK�AKAI��AI33AH�AH$�AFM�AEx�AE;dAD�yADQ�AC�#AC��AB��A@��A@E�A@JA?�7A?A>ĜA>~�A=7LA;x�A9S�A8ĜA8��A8bNA8  A7�A6r�A6  A5�hA533A4�uA3�A3|�A2n�A2 �A2A1�mA1\)A/��A.��A.��A-��A,��A+��A*�+A)ƨA(�HA(I�A'�TA'XA&VA%��A$VA#O�A#"�A#�A"��A �!A�A/A
=A�9A�#Ap�AC�A
=A�A�^AO�A�TA�AM�A33A�A��AffA�AK�AĜA��A%A-A��A�hA�AQ�Ax�A��A��AE�A��AĜA�\AI�A �A�
A��AC�A
(�A	�A	G�A��A�+A��A��A��A�A;dA��A�yA�HAȴAz�A�wA%A1'A �yA ��A Q�@��@��#@��@�1@��@�G�@���@�p�@�V@�ƨ@���@�b@띲@�\)@�&�@��H@���@�1'@��H@���@�hs@�@�bN@���@�?}@�33@٩�@�j@ו�@�~�@���@�p�@�  @ҏ\@ёh@�bN@Ͼw@�"�@�hs@��#@�`B@ț�@ǝ�@Ɨ�@��T@�\)@�/@��m@��!@�%@� �@��+@�J@��@���@�~�@�{@�V@��9@�1'@�t�@�n�@�V@�A�@���@�=q@�/@��@�
=@�ff@�p�@��9@���@�o@���@��y@���@��R@���@�M�@���@�7L@�Ĝ@�^5@�G�@���@�Ĝ@���@��m@�t�@��@�{@��@�?}@��/@�z�@���@�S�@��y@���@�5?@��@��@��@�Ĝ@���@�Q�@�1'@�  @���@�t�@��@�J@�`B@��j@�I�@�(�@���@�l�@�C�@��@�5?@���@�p�@��@�Ĝ@��9@��@��u@�I�@�b@�ƨ@��@�33@���@�ff@��@��7@�`B@���@�bN@���@��F@��P@�"�@��y@���@�v�@�V@�{@��h@��D@��;@��F@��P@�
=@��R@�^5@�5?@�J@���@�hs@��@�A�@��@��F@���@���@���@��@�S�@��y@���@���@��+@�^5@���@���@��-@��h@��7@��@�?}@�?}@�V@�Ĝ@�Ĝ@���@���@��u@�j@�z�@�z�@�Z@�@~ȴ@}�-@}V@|j@{�
@{t�@z�H@z�!@zM�@z-@z-@z-@yhs@xbN@w�@w��@w|�@wl�@wK�@w;d@w
=@v��@v��@v5?@v5?@u��@u�h@up�@u`B@u/@t��@t��@t�@s33@r��@r�@q��@qG�@q7L@q7L@q&�@p�`@pbN@p1'@o�w@n��@nv�@n@m?}@mV@l��@l�@k�
@k�F@k��@kC�@j�@j�!@j~�@jn�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��A���A���A���A���A���A���A���A���A��TA��;A��TA��A���A��9A�z�A���A��A�z�A�A���A�n�A�"�A���A�  A���A�hsA�(�A��A�ƨA�Q�A���A��
A��RA���A��uA�M�A�+A�JA��wA�l�A�&�A��mA��RA�1'A���A�VA���A��yA��HA���A���A�/A���A�n�A�9XA�ƨA���A�Q�A��#A��A��A�&�A�+A��HA���A�|�A�?}A�r�A�$�A�XA���A��7A���A�hsA�A��+A��A�-A���A�1'A��\A�A��DA���A�{A�VA���A�z�A�;dA���A��`A��^A���A�G�A�p�A�\)A�(�A��\A�jA��A�$�A~��A{�TAy?}Av^5At�jAs|�Ar-Ao�hAn��Am�mAlQ�Aj�Ah�`Ah��Ahz�Ag�7AehsAd�uAc+A_+A]O�AZ�yAWAU�TAS��AQ?}AOO�AM�AK�AKAI��AI33AH�AH$�AFM�AEx�AE;dAD�yADQ�AC�#AC��AB��A@��A@E�A@JA?�7A?A>ĜA>~�A=7LA;x�A9S�A8ĜA8��A8bNA8  A7�A6r�A6  A5�hA533A4�uA3�A3|�A2n�A2 �A2A1�mA1\)A/��A.��A.��A-��A,��A+��A*�+A)ƨA(�HA(I�A'�TA'XA&VA%��A$VA#O�A#"�A#�A"��A �!A�A/A
=A�9A�#Ap�AC�A
=A�A�^AO�A�TA�AM�A33A�A��AffA�AK�AĜA��A%A-A��A�hA�AQ�Ax�A��A��AE�A��AĜA�\AI�A �A�
A��AC�A
(�A	�A	G�A��A�+A��A��A��A�A;dA��A�yA�HAȴAz�A�wA%A1'A �yA ��A Q�@��@��#@��@�1@��@�G�@���@�p�@�V@�ƨ@���@�b@띲@�\)@�&�@��H@���@�1'@��H@���@�hs@�@�bN@���@�?}@�33@٩�@�j@ו�@�~�@���@�p�@�  @ҏ\@ёh@�bN@Ͼw@�"�@�hs@��#@�`B@ț�@ǝ�@Ɨ�@��T@�\)@�/@��m@��!@�%@� �@��+@�J@��@���@�~�@�{@�V@��9@�1'@�t�@�n�@�V@�A�@���@�=q@�/@��@�
=@�ff@�p�@��9@���@�o@���@��y@���@��R@���@�M�@���@�7L@�Ĝ@�^5@�G�@���@�Ĝ@���@��m@�t�@��@�{@��@�?}@��/@�z�@���@�S�@��y@���@�5?@��@��@��@�Ĝ@���@�Q�@�1'@�  @���@�t�@��@�J@�`B@��j@�I�@�(�@���@�l�@�C�@��@�5?@���@�p�@��@�Ĝ@��9@��@��u@�I�@�b@�ƨ@��@�33@���@�ff@��@��7@�`B@���@�bN@���@��F@��P@�"�@��y@���@�v�@�V@�{@��h@��D@��;@��F@��P@�
=@��R@�^5@�5?@�J@���@�hs@��@�A�@��@��F@���@���@���@��@�S�@��y@���@���@��+@�^5@���@���@��-@��h@��7@��@�?}@�?}@�V@�Ĝ@�Ĝ@���@���@��u@�j@�z�@�z�@�Z@�@~ȴ@}�-@}V@|j@{�
@{t�@z�H@z�!@zM�@z-@z-@z-@yhs@xbN@w�@w��@w|�@wl�@wK�@w;d@w
=@v��@v��@v5?@v5?@u��@u�h@up�@u`B@u/@t��@t��@t�@s33@r��@r�@q��@qG�@q7L@q7L@q&�@p�`@pbN@p1'@o�w@n��@nv�@n@m?}@mV@l��@l�@k�
@k�F@k��@kC�@j�@j�!@j~�@jn�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB+BPB\B\B\B\BbB\BbBbBhBbBbBbBbB\B�B"�B)�B33B6FB=qBE�BH�BbNB�B�1B�=B�JB�VB�oB�oB�oB�hB�hB�hB�oB�oB�oB�uB��B��B��B��B��B��B��B��B�{B�{B�oB�VB�DB�7B�JB�JB�7Bz�Br�BhsBR�B;dB'�B�B�B�BPB  B��B�B�yB�HB��BÖB�^B�-B�B��B��B��B�PB�B|�Bu�BjB_;BS�BK�BB�B-BoBB
�B
��B
�dB
�B
�B
��B
��B
�PB
�B
t�B
]/B
E�B
2-B
�B
bB
+B	��B	�B	�sB	�TB	�B	��B	��B	�}B	�qB	�?B	��B	��B	�{B	z�B	p�B	aHB	P�B	E�B	:^B	.B	#�B	�B	uB	bB	PB	JB	JB	
=B	B	B	  B��B��B��B��B��B�B�B�B�B�yB�mB�`B�/B��BƨBĜBĜBƨBɺB��B��B��BɺBǮBĜBÖB��B�}B�wB�wB�jB�XB�3B�B�B��B��B��B��B��B�uB�hB�\B�PB�=B�%B�B�B� B~�B{�Bw�Bt�Bs�Br�Bq�Bo�Bn�Bn�Bm�Bk�BiyBgmBe`BcTB`BB^5B^5B]/B\)BZBYBW
BT�BQ�BP�BO�BN�BL�BJ�BI�BG�BF�BE�BD�BB�BB�BA�BA�B@�B?}B>wB<jB;dB:^B9XB8RB6FB49B33B2-B1'B1'B1'B0!B0!B/B-B+B(�B'�B&�B&�B$�B#�B"�B �B�B�B�B�B�B�B�B�B�B{BuBoBhBbBbBbBbBbB\BVBVBVB\B\B\BbBbB\B\BbBbBbBbBbB\BuBuB{B�B�B{B�B�B�B�B�B�B�B �B"�B#�B$�B$�B&�B&�B&�B'�B)�B-B/B33B49B7LB<jB>wB?}BC�BF�BK�BM�BM�BN�BN�BN�BN�BO�BP�BR�BR�B\)BaHBcTBdZBdZBhsBiyBjBn�Bq�Br�Bt�Bv�Bx�B{�B~�B� B�B�B�%B�1B�=B�DB�PB�PB�VB�hB�hB�uB��B��B��B��B��B��B��B�B�B�3B�RB�XB�dB�wB�wB�wB�}B��BBĜBŢBǮBȴB��B��B��B��B�B�#B�;B�NB�TB�fB�sB�B�B�B�B�B��B��B��B	B	%B		7B	PB	VB	bB	�B	�B	�B	�B	"�B	%�B	'�B	(�B	)�B	+B	,B	-B	/B	0!B	0!B	1'B	6FB	6FB	7LB	8RB	9XB	9XB	=qB	>wB	?}B	A�B	B�B	D�B	D�B	I�B	M�B	N�B	N�B	O�B	P�B	S�B	W
B	YB	[#B	]/B	_;B	cTB	dZB	e`B	e`B	e`B	ffB	hsB	n�B	q�B	q�B	r�B	r�B	s�B	s�B	t�B	t�B	v�B	y�B	y�B	{�B	~�B	~�B	~�B	� B	�B	�B	�B	�+B	�7B	�PB	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�'B	�-B	�-B	�3B	�?B	�LB	�XB	�X111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BB
=BJBJBJBJBPBJBPBPBVBPBPBPBVBVB�B �B'�B0!B49B;dBC�BH�BaHB� B�B�1B�7B�JB�bB�\B�\B�VB�VB�\B�\B�\B�bB�hB�uB�{B�{B�uB�uB�oB�uB�oB�hB�oB�hB�PB�7B�%B�=B�DB�1Bx�Bp�BhsBR�B;dB%�B�B�B�BPB��B�B�B�sB�HB��B��B�RB�!B�B��B��B�{B�DB�Bz�Bt�BiyB^5BQ�BJ�BB�B.BbBB
�B
��B
�XB
�B
��B
��B
��B
�DB
� B
u�B
^5B
E�B
2-B
�B
\B
%B	��B	�B	�fB	�NB	�
B	ɺB	�wB	�qB	�jB	�?B	��B	��B	��B	x�B	q�B	bNB	P�B	E�B	:^B	-B	"�B	�B	bB	\B	DB	
=B	
=B	
=B	B��B��B��B��B��B��B��B�B�B�B�B�fB�`B�ZB�)B��BÖB��BBĜBǮBɺB��BɺBǮBŢBB��B�}B�jB�dB�dB�^B�RB�'B�B�B��B��B��B��B�{B�hB�\B�PB�DB�1B�B�B}�B|�B|�B{�Bu�Bq�Bp�Bp�Bo�Bl�Bk�Bl�Bk�BjBgmBffBbNBbNB^5B\)B[#BZBZBXBW
BT�BR�BO�BN�BL�BL�BJ�BH�BG�BD�BD�BC�BB�B?}B?}B>wB?}B=qB=qB=qB9XB9XB8RB7LB6FB5?B2-B0!B/B.B.B.B-B.B-B+B(�B'�B$�B$�B$�B#�B �B �B�B�B�B�B�B�B{BuBoBuBuBoBbB\BVBVBPBVBPBPBPBJBJBPBPBPBPBVBPBPBVBVBPBVB\BVBbBhBoBuBuBuBuB�B�B�B�B�B�B�B �B!�B!�B"�B#�B$�B$�B%�B'�B+B-B0!B2-B5?B9XB<jB=qBA�BD�BH�BJ�BJ�BK�BK�BK�BK�BL�BN�BP�BQ�BZB^5B`BBaHBbNBe`BgmBhsBk�Bn�Bo�Bq�Bt�Bv�Bx�B{�B|�B~�B�B�B�B�+B�1B�=B�=B�DB�VB�\B�hB��B��B��B��B��B��B��B��B��B�!B�?B�FB�RB�dB�dB�dB�jB�wB�}B��BBĜBƨB��B��B��B��B�B�B�)B�;B�BB�TB�`B�mB�sB�yB�B�B��B��B��B��B	B	%B	
=B	DB	PB	uB	�B	�B	�B	�B	"�B	$�B	%�B	&�B	'�B	(�B	)�B	,B	-B	-B	.B	33B	33B	49B	5?B	6FB	6FB	:^B	;dB	<jB	>wB	?}B	A�B	A�B	F�B	J�B	K�B	K�B	L�B	N�B	P�B	S�B	VB	XB	ZB	\)B	`BB	aHB	bNB	bNB	bNB	cTB	e`B	k�B	n�B	n�B	o�B	o�B	p�B	p�B	q�B	q�B	s�B	v�B	v�B	x�B	{�B	{�B	{�B	|�B	}�B	}�B	� B	�B	�%B	�=B	�VB	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�-B	�9B	�FB	�F111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - (REPORTED_SURFACE_PRESSURE) for PADJ                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            PSAL_ADJ corrects CTD thermal lag (CTL) viz. Johnson et al, 2007, JAOT, effects of pressure adjustments, and PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment term r.                  PADJ REPORTED_SURFACE_PRESSURE =0.07 dbar                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CTL alpha = 0.021 & tau = 21 s with error equal to |correction| and for OW r = 0.9999 (+/-0), vertically averaged dS = -0.003 (+/-0.001)                                                                                                                        Pressures adjusted using reported surface pressure when warranted.                                                                                                                                                                                              none                                                                                                                                                                                                                                                            After pressure and cell thermal lag correction of salinity values, OW correction estimated using mapping scales of 8 & 4 long. and 4 & 2 lat., no PV constraint, and decorrelation time scale of 10 years.                                                      202307212300272023072123002720230721230027  AO  ARCAADJP                                                                    20200330120030    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200330120030  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200330120030  QCF$                G�O�G�O�G�O�0               PM  ARSQPADJV1.1                                                                20230712105755  QC  PRES            @�33D}��G�O�                PM  ARSQCTM V1.1                                                                20230712105755  QC  PSAL            @�33D}��G�O�                PM  ARSQCOWGV1.1CTD_2021v2 + Argo_2021v03                                       20230721230926  IP                  G�O�G�O�G�O�                