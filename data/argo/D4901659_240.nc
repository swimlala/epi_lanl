CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2021-02-23T08:00:33Z creation      
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
resolution        =���   axis      Z          9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���       C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���       M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   h   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       j   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       r8   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   zT   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       |\   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   �x   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �    HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �H   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �X   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �\   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �l   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �p   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �t   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �x        �xArgo profile    3.1 1.2 19500101000000  20210223080033  20230721230930  4901659 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               �A   AO  5382                            2C  D   NAVIS_A                         0371                            082713                          863 @�`��Sy�1   @�`�%�i@;�t�j~��d(r� Ĝ1   GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                      �A   A   A   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D fD � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dl��Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�\)@�@�A�HA>�HA^�HA~�HA�p�A�p�A�p�A�p�A�p�A�p�A�p�A�p�B�RB�RB�RB�RB'�RB/�RB7�RB?�RBG�RBO�RBW�RB_�RBg�RBo�RBw�RB�RB��)B��)B���B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)C�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��
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
D {�D ��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D	{�D	��D
{�D
��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D �D {�D ��D!{�D!��D"{�D"��D#{�D#��D${�D$��D%{�D%��D&{�D&��D'{�D'��D({�D(��D){�D)��D*{�D*��D+{�D+��D,{�D,��D-{�D-��D.{�D.��D/{�D/��D0{�D0��D1{�D1��D2{�D2��D3{�D3��D4{�D4��D5{�D5��D6{�D6��D7{�D7��D8{�D8��D9{�D9��D:{�D:��D;{�D;��D<{�D<��D={�D=��D>{�D>��D?{�D?��D@{�D@��DA{�DA��DB{�DB��DC{�DC��DD{�DD��DE{�DE��DF{�DF��DG{�DG��DH{�DH��DI{�DI��DJ{�DJ��DK{�DK��DL{�DL��DM{�DM��DN{�DN��DO{�DO��DP{�DP��DQ{�DQ��DR{�DR��DS{�DS��DT{�DT��DU{�DU��DV{�DV��DW{�DW��DX{�DX��DY{�DY��DZ{�DZ��D[{�D[��D\{�D\��D]{�D]��D^{�D^��D_{�D_��D`{�D`��Da{�Da��Db{�Db��Dc{�Dc��Dd{�Dd��De{�De��Df{�Df��Dg{�Dg��Dh{�Dh��Di{�Di��Dj{�Dj��Dk{�Dk��Dl{�Dl�Dm{�Dm��Dn{�Dn��Do{�Do��Dp{�Dp��Dq{�Dq��Dr{�Dr��Ds{�Ds��Dt{�Dt��Du{�Du��Dv{�Dv��Dw{�Dw��Dx{�Dx��Dy{�Dy��Dz{�Dz��D{{�D{��D|{�D|��D}{�D}��D~{�D~��D{�D��D�=�D�}�D���D���D�=�D�}�D���D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A�"�A��A��A��A��A��A�$�A��A�&�A�oA��#A���A���A��uA��uA��hA��PA��\A��7A��A�z�A�bNA�XA�VA�VA�S�A�S�A�=qA��A��A�jA���A���A�S�A���A�(�A��A�bNA�1'A���A�n�A���A�t�A�ƨA�M�A���A�A��A�ƨA��A��!A�A�O�A�(�A�-A���A��jA�1'A��A���A���A��A��A�A�l�A�A���A�^5A�5?A���A�ffA�K�A�+A�E�A�dZA��HA�Q�A��!A�?}A��A�33A�?}A�?}A��A��9A�VA���A��mA��TA��^A���A��A�K�A�I�A�ȴA�VA�
=A��A
=A}l�A{��Az�AzI�Az1Ay�^Ay7LAxjAw&�At��AtZAr��Aq�ApbNAo�AnI�Am
=Al1AjĜAh�9Ae�AchsAa�AaG�A`�A`��A` �A]��A\�DA\�\A\�+A[�7AZ�9AZ�AW�FAT��ATJAS7LAQ�FAO��ANr�AL^5AJ�jAJJAI�AH�yAH1AF��AF1AD�RAC��AB�AA�TA@�RA@bNA@5?A?��A?S�A=��A<=qA;�A9��A9x�A8�A81'A6$�A4�uA3�-A3O�A3&�A3�A3oA3%A2�HA1��A1`BA0��A0M�A/?}A.JA-�A,��A,��A+�-A*9XA)�-A)\)A(VA'��A&ĜA&bA%��A%;dA$�A$9XA#��A#ƨA#|�A"��A"�\A"=qA!�TA ��A��A�AM�A1'A1A�A�
A�hA%AA�AƨAx�AG�A&�Av�AJA��A�A�yA��AbAx�A7LAĜA�AoAI�AC�Ar�AbA��A�A
v�A	�#A	|�A��AVA=qA�;AS�A��An�A��A�A�TA��A =qA  �@�K�@�G�@�b@�|�@��@��@�  @�E�@��m@�dZ@�R@���@��@�;d@�ȴ@�V@�  @�ff@��/@�ƨ@��@�?}@�dZ@���@�j@�$�@��#@ݑh@�`B@��`@���@�33@��@ڏ\@�=q@��@��T@ٲ-@�/@���@��@���@�Z@ץ�@��#@�ff@�Q�@�/@�  @�+@���@�n�@ɡ�@�V@� �@�o@�E�@�X@�b@��@�S�@�r�@��R@�V@�=q@��T@���@���@�Q�@�|�@���@�hs@���@�\)@�$�@�X@��@��@���@���@�A�@�b@��;@���@�t�@��H@��!@�^5@�7L@��j@�33@��@���@��u@�Q�@��@��@�o@�E�@���@�1@�
=@��@���@�Z@�(�@�b@��m@��F@�l�@�33@���@�ff@��^@��@��9@��;@�\)@�S�@�S�@�+@�=q@���@��7@�V@���@��@�I�@��@�\)@�o@��H@���@�V@��#@��@�O�@�7L@�&�@���@��9@���@�z�@�I�@�  @��w@���@��@�+@���@���@�-@�O�@�Z@��w@��P@�|�@�t�@�dZ@�S�@�;d@�33@�"�@���@�~�@��T@�`B@�%@��9@�z�@�1'@��m@��F@���@���@���@�|�@�"�@���@��@��y@��R@�E�@��@�@��T@���@�p�@�7L@�&�@���@�Ĝ@��u@�t�@���@�M�@�-@��@��T@��-@�x�@�/@��@��@��`@��j@��D@�bN@�I�@�9X@�1@�P@\)@K�@+@~V@}�-@}�h@}p�@|�@|��@|�@|z�@|j@|9X@|1@{ƨ@{��@{�@{dZ@{�@{S�@{@z��@zn�@y�#@y�7@xĜ@xQ�@xb@w��@w�w@w�w@w�w@w��@w�w@wl�@v��@t�@st�@sC�@r�!@q�^@o�@o�@n�R@nv�@n{@m�T@m@m�-@m�@mO�@m/@m/@l��@l�j@k�m111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��A�"�A��A��A��A��A��A�$�A��A�&�A�oA��#A���A���A��uA��uA��hA��PA��\A��7A��A�z�A�bNA�XA�VA�VA�S�A�S�A�=qA��A��A�jA���A���A�S�A���A�(�A��A�bNA�1'A���A�n�A���A�t�A�ƨA�M�A���A�A��A�ƨA��A��!A�A�O�A�(�A�-A���A��jA�1'A��A���A���A��A��A�A�l�A�A���A�^5A�5?A���A�ffA�K�A�+A�E�A�dZA��HA�Q�A��!A�?}A��A�33A�?}A�?}A��A��9A�VA���A��mA��TA��^A���A��A�K�A�I�A�ȴA�VA�
=A��A
=A}l�A{��Az�AzI�Az1Ay�^Ay7LAxjAw&�At��AtZAr��Aq�ApbNAo�AnI�Am
=Al1AjĜAh�9Ae�AchsAa�AaG�A`�A`��A` �A]��A\�DA\�\A\�+A[�7AZ�9AZ�AW�FAT��ATJAS7LAQ�FAO��ANr�AL^5AJ�jAJJAI�AH�yAH1AF��AF1AD�RAC��AB�AA�TA@�RA@bNA@5?A?��A?S�A=��A<=qA;�A9��A9x�A8�A81'A6$�A4�uA3�-A3O�A3&�A3�A3oA3%A2�HA1��A1`BA0��A0M�A/?}A.JA-�A,��A,��A+�-A*9XA)�-A)\)A(VA'��A&ĜA&bA%��A%;dA$�A$9XA#��A#ƨA#|�A"��A"�\A"=qA!�TA ��A��A�AM�A1'A1A�A�
A�hA%AA�AƨAx�AG�A&�Av�AJA��A�A�yA��AbAx�A7LAĜA�AoAI�AC�Ar�AbA��A�A
v�A	�#A	|�A��AVA=qA�;AS�A��An�A��A�A�TA��A =qA  �@�K�@�G�@�b@�|�@��@��@�  @�E�@��m@�dZ@�R@���@��@�;d@�ȴ@�V@�  @�ff@��/@�ƨ@��@�?}@�dZ@���@�j@�$�@��#@ݑh@�`B@��`@���@�33@��@ڏ\@�=q@��@��T@ٲ-@�/@���@��@���@�Z@ץ�@��#@�ff@�Q�@�/@�  @�+@���@�n�@ɡ�@�V@� �@�o@�E�@�X@�b@��@�S�@�r�@��R@�V@�=q@��T@���@���@�Q�@�|�@���@�hs@���@�\)@�$�@�X@��@��@���@���@�A�@�b@��;@���@�t�@��H@��!@�^5@�7L@��j@�33@��@���@��u@�Q�@��@��@�o@�E�@���@�1@�
=@��@���@�Z@�(�@�b@��m@��F@�l�@�33@���@�ff@��^@��@��9@��;@�\)@�S�@�S�@�+@�=q@���@��7@�V@���@��@�I�@��@�\)@�o@��H@���@�V@��#@��@�O�@�7L@�&�@���@��9@���@�z�@�I�@�  @��w@���@��@�+@���@���@�-@�O�@�Z@��w@��P@�|�@�t�@�dZ@�S�@�;d@�33@�"�@���@�~�@��T@�`B@�%@��9@�z�@�1'@��m@��F@���@���@���@�|�@�"�@���@��@��y@��R@�E�@��@�@��T@���@�p�@�7L@�&�@���@�Ĝ@��u@�t�@���@�M�@�-@��@��T@��-@�x�@�/@��@��@��`@��j@��D@�bN@�I�@�9X@�1@�P@\)@K�@+@~V@}�-@}�h@}p�@|�@|��@|�@|z�@|j@|9X@|1@{ƨ@{��@{�@{dZ@{�@{S�@{@z��@zn�@y�#@y�7@xĜ@xQ�@xb@w��@w�w@w�w@w�w@w��@w�w@wl�@v��@t�@st�@sC�@r�!@q�^@o�@o�@n�R@nv�@n{@m�T@m@m�-@m�@mO�@m/@m/@l��@l�j@k�m111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�^B�^B�XB�XB�XB�XB�XB�XB�XB�XB�XB�LB�FB�FB�FB�FB�FB�FB�FB�FB�FB�FB�FB�LB�LB�LB�LB�LB�RB�RB�XB�RB�?B�-B�B��B��B�B�LB�FB�3B�B��B��Bv�BbNBK�B>wB.B�BhB	7BB��B�B�B�qB�!B��B��B�hBm�BhsBffBdZB`BBYBP�BI�B7LB�B	7B%BB
��B
�B
�B
�B
�yB
�NB
��B
B
�LB
�B
�B
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�bB
�7B
}�B
m�B
[#B
O�B
C�B
=qB
9XB
8RB
6FB
2-B
,B
"�B
�B
�B

=B
B	��B	��B	�B	�yB	�ZB	�#B	��B	�qB	�FB	�-B	�B	�B	��B	��B	�bB	�=B	�DB	�JB	�7B	�B	� B	m�B	R�B	K�B	F�B	>wB	2-B	-B	"�B	�B	�B	!�B	"�B	�B	�B	oB	PB	+B	B	B	B	  B	  B��B��B��B�B�B�sB�sB�sB�ZB�5B�B��B��B��B��B��B��B��B��BɺBƨBÖB�qB�^B�XB�FB�9B�'B�B��B��B��B��B��B��B��B��B��B��B�{B�{B�oB�bB�\B�PB�=B�B~�B{�Bz�By�By�By�Bx�Bw�Bu�Bs�Bq�Bp�Bp�Bo�Bm�Bk�BjBhsBe`B_;BZBYBW
BVBS�BQ�BN�BM�BK�BJ�BI�BH�BF�BE�BD�BC�BB�BA�B@�B>wB;dB8RB6FB49B33B1'B1'B0!B/B.B-B,B+B)�B(�B'�B&�B%�B%�B$�B#�B"�B"�B �B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B"�B$�B&�B-B.B.B-B0!B1'B2-B5?B6FB9XB<jB?}B?}B?}B?}B?}BA�BB�BB�BB�BC�BE�BF�BG�BK�BL�BQ�BXBZB[#B\)B]/B^5B`BBdZBiyBm�Br�B|�B~�B� B�B�B�B�B�B�%B�1B�7B�JB�\B�bB�{B��B��B��B��B��B��B��B��B��B��B�B�B�!B�-B�3B�9B�?B�RB�^B�dB�dB�dB�jB�wB�}B��BBÖBŢBŢBƨB��B��B��B��B�
B�5B�NB�TB�TB�ZB�ZB�ZB�`B�fB�fB�mB�B�B��B��B��B��B��B	B	B	B	B	B	%B	1B	
=B	
=B	DB	JB	\B	hB	hB	oB	{B	�B	�B	�B	�B	�B	�B	&�B	.B	0!B	1'B	33B	33B	5?B	6FB	9XB	:^B	:^B	;dB	=qB	>wB	@�B	@�B	A�B	D�B	G�B	H�B	H�B	I�B	M�B	P�B	P�B	Q�B	S�B	T�B	VB	W
B	W
B	XB	XB	ZB	ZB	[#B	\)B	[#B	\)B	]/B	^5B	`BB	cTB	dZB	hsB	jB	k�B	l�B	m�B	m�B	m�B	m�B	m�B	n�B	o�B	y�B	�B	�B	�B	�1B	�bB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�-B�-B�'B�'B�'B�'B�'B�'B�'B�-B�-B�!B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�!B�'B�'B�3B�-B�!B�B��B��B��B��B�!B�B�B��B��B�oBr�B^5BE�B:^B)�BPBDBB��B��B�yB�B�LB��B��B��B�bBdZBaHB_;B]/BZBR�BJ�BE�B6FBoBB
��B
��B
�B
�fB
�mB
�fB
�TB
�5B
ȴB
�qB
�'B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�oB
�\B
�\B
�DB
�B
x�B
iyB
T�B
I�B
<jB
6FB
2-B
1'B
/B
,B
&�B
�B
hB
hB
B	��B	��B	��B	�B	�TB	�;B	�
B	��B	�RB	�!B	�B	��B	��B	��B	��B	�7B	�B	�B	�%B	�B	~�B	|�B	iyB	K�B	E�B	A�B	9XB	,B	'�B	�B	�B	�B	�B	�B	�B	hB	JB	+B	B��B��B��B��B��B��B��B�B�B�fB�BB�NB�NB�BB�B��B��B��B��B��B��B��BɺBĜBB��B�qB�LB�3B�3B�B�B�B��B��B��B��B��B��B��B�uB�oB�bB�\B�PB�PB�DB�7B�1B�+B�B�Bx�Bt�Bs�Br�Br�Br�Bq�Bq�Bo�Bl�BjBiyBiyBiyBffBdZBcTBbNBaHBZBQ�BQ�BO�BO�BM�BK�BH�BG�BD�BC�BB�BB�B?}B>wB>wB<jB;dB;dB:^B9XB5?B2-B0!B-B,B+B(�B)�B(�B&�B%�B%�B#�B"�B"�B!�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B{B{B{BoBuBuBoBoBoBoBoBoBoBoBoBoBuBuBoBoBuBhBhBhBhBoBoBoBoBoBuB{BuB{B�B�B�B�B�B�B�B!�B%�B&�B&�B&�B(�B)�B,B.B0!B2-B5?B8RB8RB8RB8RB8RB:^B;dB;dB;dB<jB>wB?}B@�BD�BF�BK�BP�BR�BS�BT�BVBW
BYB^5BbNBgmBl�Bu�Bw�Bx�By�Bz�B{�B|�B}�B~�B�B�B�B�1B�7B�PB�bB�bB�bB�bB��B��B��B��B��B��B��B��B��B�B�B�B�B�'B�3B�9B�9B�9B�?B�LB�RB�^B�dB�jB�wB�wB�}BÖBŢBƨBɺB��B�
B�#B�)B�)B�/B�/B�/B�5B�;B�;B�BB�ZB�B�B�B�B��B��B��B��B��B��B��B��B	B	B	B	B	B	1B	
=B	
=B	DB	PB	VB	bB	hB	oB	{B	�B	�B	&�B	(�B	)�B	,B	,B	.B	/B	2-B	33B	33B	49B	6FB	7LB	9XB	9XB	:^B	=qB	@�B	A�B	A�B	B�B	F�B	I�B	I�B	J�B	L�B	M�B	N�B	O�B	O�B	P�B	P�B	R�B	R�B	S�B	T�B	S�B	T�B	VB	W
B	YB	\)B	]/B	aHB	cTB	dZB	e`B	ffB	ffB	ffB	ffB	ffB	gmB	iyB	r�B	y�B	y�B	|�B	�B	�7B	�PB	�VB	�\B	�hB	�oB	�oB	�uB	�uB	��B	��B	��B	��B	��B	��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 =m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`BPRES            TEMP            PSAL            PRES_ADJUSTED = PRES - (REPORTED_SURFACE_PRESSURE) for PADJ                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            PSAL_ADJ corrects CTD thermal lag (CTL) viz. Johnson et al, 2007, JAOT, effects of pressure adjustments, and PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment term r.                  PADJ REPORTED_SURFACE_PRESSURE =0.07 dbar                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CTL alpha = 0.021 & tau = 21 s with error equal to |correction| and for OW r = 0.9998 (+/-0.0014), vertically averaged dS = -0.007 (+/-0.055)                                                                                                                   Pressures adjusted using reported surface pressure when warranted.                                                                                                                                                                                              none                                                                                                                                                                                                                                                            After pressure and cell thermal lag correction of salinity values, OW correction estimated using mapping scales of 8 & 4 long. and 4 & 2 lat., no PV constraint, and decorrelation time scale of 10 years.                                                      202307212300302023072123003020230721230030  AO  ARCAADJP                                                                    20210223080033    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20210223080033  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20210223080033  QCF$                G�O�G�O�G�O�0               PM  ARSQPADJV1.1                                                                20230712105813  QC  PRES            @���D�  G�O�                PM  ARSQCTM V1.1                                                                20230712105813  QC  PSAL            @���D�  G�O�                PM  ARSQCOWGV1.1CTD_2021v2 + Argo_2021v03                                       20230721230930  IP                  G�O�G�O�G�O�                