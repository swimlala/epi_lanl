CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-11-25T13:00:37Z creation      
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
resolution        =���   axis      Z          9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���       C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���       M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       i�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       q�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   y�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       {�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �H   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �H   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �H   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �H   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �t   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �x   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �|   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��        ��Argo profile    3.1 1.2 19500101000000  20201125130037  20230721230929  4901659 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               �A   AO  5382                            2C  D   NAVIS_A                         0371                            082713                          863 @�J���o1   @�J)���@<>��"���d�$�/1   GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                      �A   A   A   @���@���A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B��B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�3D�9�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��\@��\@�A�HA>�HA^�HA~�HA�p�A�p�A�p�A�p�A�p�A�p�A�p�A�p�B�RB�RB�RB�RB'�RB/�RB7�RB?�RBG�RBO�RBW�RB_�RBg�RBo�RBw�RBQ�B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)C�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��
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
C��C��
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
C��C��
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
��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D {�D ��D!{�D!��D"{�D"��D#{�D#��D${�D$��D%{�D%��D&{�D&��D'{�D'��D({�D(��D){�D)��D*{�D*��D+{�D+��D,{�D,��D-{�D-��D.{�D.��D/{�D/��D0{�D0��D1{�D1��D2{�D2��D3{�D3��D4{�D4��D5{�D5��D6{�D6��D7{�D7��D8{�D8��D9{�D9��D:{�D:��D;{�D;��D<{�D<��D={�D=��D>{�D>��D?{�D?��D@{�D@��DA{�DA��DB{�DB��DC{�DC��DD{�DD��DE{�DE��DF{�DF��DG{�DG��DH{�DH��DI{�DI��DJ{�DJ��DK{�DK��DL{�DL��DM{�DM��DN{�DN��DO{�DO��DP{�DP��DQ{�DQ��DR{�DR��DS{�DS��DT{�DT��DU{�DU��DV{�DV��DW{�DW��DX{�DX��DY{�DY��DZ{�DZ��D[{�D[��D\{�D\��D]{�D]��D^{�D^��D_{�D_��D`{�D`��Da{�Da��Db{�Db��Dc{�Dc��Dd{�Dd��De{�De��Df{�Df��Dg{�Dg��Dh{�Dh��Di{�Di��Dj{�Dj��Dk{�Dk��Dl{�Dl��Dm{�Dm��Dn{�Dn��Do{�Do��Dp{�Dp��Dq{�Dq��Dr{�Dr��Ds{�Ds��Dt{�Dt��Du{�Du��Dv{�Dv��Dw{�Dw��Dx{�Dx��Dy{�Dy��Dz{�Dz��D{{�D{��D|{�D|��D}{�D}��D~{�D~��D{�D��D�=�D�}�D���D� �D�7]111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�+A�/A�7LA�5?A�5?A�7LA�5?A�7LA�33A�33A�&�A�$�A�JA���A��;A��A���A�A��-A�r�A�G�A�C�A�9XA�-A�"�A�oA��yA�G�A��jA���A�hsA�O�A�?}A���A�^5A���A�A�A�?}A��A��TA�\)A�jA��HA�p�A�33A�z�A�~�A��mA�r�A���A��A��#A��-A��uA�ffA�;dA�ȴA�~�A���A�"�A��
A�bNA���A��-A���A��/A�VA�^5A�ffA�E�A��FA�n�A��A���A�A�A���A�(�A��DA�+A��AG�A};dA|bNAy�^AwC�AuƨAsdZAr1'Aq�Apv�An�Al�+Aj��Aj9XAi��Ai�Ahr�Ah{AgK�Af~�Ae"�Ac��Ab�!Ab�+Abr�Aa�
A`ZA\1'A[%AZ�HAZ�RAY�;AX��AXbAVZAU�PAT�/AT�\AT$�AR�9AQ�hAP��APE�AO�AO�-AO|�AO
=AN$�AK��AJ��AI�mAI��AHjAG��AG�7AG+AFI�AE��AE/AD�+AD5?ACx�ABVAA
=A@r�A?�mA?C�A?&�A?
=A>�yA>�HA>ȴA>v�A=��A<  A;��A;�A9��A8-A6�!A5ƨA4�A4r�A4�A3|�A2�A29XA0��A09XA/�7A/S�A//A.�`A.ZA.  A-C�A,~�A,=qA+�mA+�-A+7LA*z�A)�A(ȴA'�;A&ĜA&�A%�PA$�9A$-A$JA#�#A#&�A"E�A!hsA ��A r�A�mA�uAVAZA��A�RA �A"�A�`A^5A+A��A9XA��AJA�DA�A�A`BA��A9XA�A�A�#A7LA�!A�A
�9A
VA	��A�mAjA-AA\)A  A�\A+@��w@�+@���@��u@��P@���@�&�@�j@��-@��h@�x�@�%@� �@�F@@�t�@�@�I�@�t�@�\)@�\)@�+@�@��y@��@���@�ȴ@ꗍ@�-@�/@��@�ȴ@�J@��T@�h@�hs@�9@ᙚ@߾w@�C�@��@�b@ڧ�@�?}@׮@�\)@�j@�o@��T@�hs@�7L@��@�z�@��m@�t�@��@�M�@�b@�C�@�
=@�ȴ@ŉ7@���@�^5@�G�@�9X@��P@�@���@�V@��@���@�`B@�/@���@��D@��D@�r�@�9X@�|�@���@��/@��m@���@�\)@�\)@�\)@��@��j@�ƨ@��!@��T@�b@�;d@�v�@��#@��@��F@�K�@��y@�V@�J@���@���@���@��@�Z@��@�+@���@�v�@�$�@��#@�p�@���@�Q�@��;@�\)@��y@�M�@���@��h@��@�A�@��P@�`B@�Ĝ@�r�@�A�@� �@��@��#@��7@�`B@�X@�O�@� �@�33@�@��y@��\@�{@��-@��h@�hs@�O�@�7L@�/@���@��@�A�@� �@���@��
@�ƨ@��w@��@�|�@�+@��y@��\@�ff@��^@�j@�(�@�(�@�(�@�(�@���@�ƨ@�"�@�^5@�`B@�G�@�/@���@���@�9X@�ƨ@�33@��y@��\@�n�@�E�@�{@�@�Q�@���@�t�@�dZ@�S�@�C�@�;d@�33@�33@�+@�"�@��@�
=@���@��H@���@���@�v�@��@���@���@�x�@��@��j@���@��@�b@}��@|I�@|�@{��@{ƨ@{@z=q@zJ@y��@y��@y��@yx�@y�7@yX@y7L@y&�@y�@y�@y%@x�`@xĜ@x�9@x�@xr�@xbN@xA�@x �@w�w@w�w@w��@wK�@v�R@vV@u�@u@u�h@up�@t�@t�@tz�@tZ@t9X@t(�@t1@s��@s�
@s��@s"�@r��@q��@p�`@pA�@o�;@oK�@o
=@n�@n{@m�h@m�@mp�@mO�@m/@l��@l�j@l9X@k��@k�
@k�F111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�+A�/A�7LA�5?A�5?A�7LA�5?A�7LA�33A�33A�&�A�$�A�JA���A��;A��A���A�A��-A�r�A�G�A�C�A�9XA�-A�"�A�oA��yA�G�A��jA���A�hsA�O�A�?}A���A�^5A���A�A�A�?}A��A��TA�\)A�jA��HA�p�A�33A�z�A�~�A��mA�r�A���A��A��#A��-A��uA�ffA�;dA�ȴA�~�A���A�"�A��
A�bNA���A��-A���A��/A�VA�^5A�ffA�E�A��FA�n�A��A���A�A�A���A�(�A��DA�+A��AG�A};dA|bNAy�^AwC�AuƨAsdZAr1'Aq�Apv�An�Al�+Aj��Aj9XAi��Ai�Ahr�Ah{AgK�Af~�Ae"�Ac��Ab�!Ab�+Abr�Aa�
A`ZA\1'A[%AZ�HAZ�RAY�;AX��AXbAVZAU�PAT�/AT�\AT$�AR�9AQ�hAP��APE�AO�AO�-AO|�AO
=AN$�AK��AJ��AI�mAI��AHjAG��AG�7AG+AFI�AE��AE/AD�+AD5?ACx�ABVAA
=A@r�A?�mA?C�A?&�A?
=A>�yA>�HA>ȴA>v�A=��A<  A;��A;�A9��A8-A6�!A5ƨA4�A4r�A4�A3|�A2�A29XA0��A09XA/�7A/S�A//A.�`A.ZA.  A-C�A,~�A,=qA+�mA+�-A+7LA*z�A)�A(ȴA'�;A&ĜA&�A%�PA$�9A$-A$JA#�#A#&�A"E�A!hsA ��A r�A�mA�uAVAZA��A�RA �A"�A�`A^5A+A��A9XA��AJA�DA�A�A`BA��A9XA�A�A�#A7LA�!A�A
�9A
VA	��A�mAjA-AA\)A  A�\A+@��w@�+@���@��u@��P@���@�&�@�j@��-@��h@�x�@�%@� �@�F@@�t�@�@�I�@�t�@�\)@�\)@�+@�@��y@��@���@�ȴ@ꗍ@�-@�/@��@�ȴ@�J@��T@�h@�hs@�9@ᙚ@߾w@�C�@��@�b@ڧ�@�?}@׮@�\)@�j@�o@��T@�hs@�7L@��@�z�@��m@�t�@��@�M�@�b@�C�@�
=@�ȴ@ŉ7@���@�^5@�G�@�9X@��P@�@���@�V@��@���@�`B@�/@���@��D@��D@�r�@�9X@�|�@���@��/@��m@���@�\)@�\)@�\)@��@��j@�ƨ@��!@��T@�b@�;d@�v�@��#@��@��F@�K�@��y@�V@�J@���@���@���@��@�Z@��@�+@���@�v�@�$�@��#@�p�@���@�Q�@��;@�\)@��y@�M�@���@��h@��@�A�@��P@�`B@�Ĝ@�r�@�A�@� �@��@��#@��7@�`B@�X@�O�@� �@�33@�@��y@��\@�{@��-@��h@�hs@�O�@�7L@�/@���@��@�A�@� �@���@��
@�ƨ@��w@��@�|�@�+@��y@��\@�ff@��^@�j@�(�@�(�@�(�@�(�@���@�ƨ@�"�@�^5@�`B@�G�@�/@���@���@�9X@�ƨ@�33@��y@��\@�n�@�E�@�{@�@�Q�@���@�t�@�dZ@�S�@�C�@�;d@�33@�33@�+@�"�@��@�
=@���@��H@���@���@�v�@��@���@���@�x�@��@��j@���@��@�b@}��@|I�@|�@{��@{ƨ@{@z=q@zJ@y��@y��@y��@yx�@y�7@yX@y7L@y&�@y�@y�@y%@x�`@xĜ@x�9@x�@xr�@xbN@xA�@x �@w�w@w�w@w��@wK�@v�R@vV@u�@u@u�h@up�@t�@t�@tz�@tZ@t9X@t(�@t1@s��@s�
@s��@s"�@r��@q��@p�`@pA�@o�;@oK�@o
=@n�@n{@m�h@m�@mp�@mO�@m/@l��@l�j@l9X@k��@k�
@k�F111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B  B  B  B��B  B  B  B��B��B��B�B�wB��BG�B��B�ZB�;B�#B�
B��B��B��B�LB�B��B��B��B��B��B�\B�1B�Bx�Bs�Bq�Bo�Bm�BjBhsBaHBQ�B?}B)�B%B
�B
�TB
��B
ŢB
�dB
�B
��B
�9B
�?B
�B
��B
�=B
�B
}�B
y�B
n�B
dZB
^5B
T�B
G�B
49B
)�B
�B
hB

=B	��B	��B	�B	�B	�B	�BB	�B	�B	��B	��B	��B	ǮB	ÖB	�wB	�XB	�3B	�B	�B	�B	��B	��B	�\B	�7B	�1B	�%B	�B	|�B	w�B	p�B	k�B	iyB	gmB	cTB	]/B	XB	T�B	R�B	P�B	O�B	N�B	K�B	G�B	>wB	;dB	8RB	6FB	33B	1'B	0!B	.B	,B	)�B	'�B	$�B	"�B	�B	�B	�B	{B	oB	bB	\B	\B	\B	VB	VB	JB		7B	B	  B��B��B�B�B�fB�TB�HB�;B�)B�B�B��B��B��B��BɺBǮBŢBÖB��B�qB�jB�^B�XB�LB�9B�B��B��B��B��B��B��B��B��B��B�uB�\B�JB�=B�1B�B� By�Bw�Bt�Bq�Bo�Bl�BjBhsBdZBbNBaHB^5BYBT�BQ�BN�BM�BL�BK�BK�BJ�BI�BH�BF�BD�BB�B@�B>wB9XB6FB49B33B0!B-B)�B'�B%�B$�B#�B"�B �B�B�B�B�B�B�B�B�B�B�B�B�B�B{B{B{B{B{B{B{BuBuBuBoBhBhBhBhBhBhBbB\BVBVBPBDBDB
=B	7B+B1B	7B
=BDBJBJBJBPBVBVBVBVBhBoBhBbBbB{B�B�B�B�B�B�B �B!�B!�B!�B!�B"�B"�B"�B"�B"�B#�B$�B'�B)�B+B,B+B+B.B2-B49B6FB8RB<jB<jB=qB>wB@�BE�BG�BH�BI�BJ�BK�BL�BO�BO�BP�BP�BR�BS�BT�BVBW
BYB\)B_;Be`BiyBk�Bn�Bo�Bq�Bs�Bv�Bx�B�B�%B�+B�1B�1B�VB�uB��B��B��B��B��B��B��B��B��B��B�B�B�B�!B�!B�!B�3B�?B�FB�LB�RB�XB�^B�^B�^B�dB�qB�}B��BBǮB��B��B��B��B��B�B�
B�/B�NB�B�B�B�B�B�B��B��B��B	B	B	B	B	%B	oB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	!�B	#�B	&�B	)�B	,B	-B	1'B	49B	5?B	6FB	:^B	G�B	P�B	Q�B	R�B	S�B	XB	]/B	_;B	`BB	bNB	bNB	cTB	bNB	cTB	dZB	e`B	e`B	e`B	ffB	gmB	gmB	hsB	iyB	iyB	jB	k�B	l�B	n�B	n�B	o�B	q�B	t�B	v�B	w�B	x�B	y�B	y�B	|�B	~�B	~�B	� B	�B	�B	�B	�B	�B	�B	�%B	�1B	�JB	�\B	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�}B��BP�B�B�/B�B�B��B��B��B�qB�3B��B��B��B��B��B�oB�7B�B|�Br�Bl�BjBhsBgmBdZBcTB^5BN�B<jB'�BB
�mB
�HB
ȴB
��B
�RB
��B
��B
�B
�!B
��B
��B
�B
|�B
w�B
t�B
hsB
^5B
YB
P�B
B�B
.B
%�B
�B
DB
%B	��B	�B	�B	�B	�fB	�#B	��B	��B	��B	ɺB	ŢB	��B	�qB	�XB	�9B	�B	��B	��B	��B	��B	��B	�+B	�B	�B	�B	|�B	v�B	r�B	jB	e`B	bNB	aHB	^5B	W
B	P�B	N�B	K�B	I�B	I�B	H�B	F�B	C�B	7LB	5?B	1'B	1'B	-B	)�B	)�B	'�B	%�B	#�B	!�B	�B	�B	�B	{B	bB	VB	JB		7B	1B	1B	1B	+B	1B	%B	B��B��B��B�B�B�ZB�BB�)B�#B�B�B��B��B��BǮBĜBÖBÖB��B�}B�qB�^B�FB�FB�3B�3B�'B�B��B��B��B��B��B��B�uB�hB�hB�bB�PB�7B�%B�B�B� Bz�Bs�Bq�Bn�Bk�BiyBe`BdZBcTB^5B\)B[#BZBS�BO�BL�BG�BG�BE�BD�BD�BC�BC�BB�BA�B>wB<jB;dB9XB49B/B.B-B+B'�B$�B!�B�B�B�B�B�B�B�B�B�B�B{B�BbBbBbBbBbBVBPBPBPBPBPBPBPBJBJBPBJBDBDB
=B
=B
=B
=B
=B
=B1B+B+B%BBBBBBBBBBB%B+B+B1B1B	7B
=BDBDBDBPB\B{B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B!�B"�B#�B$�B#�B$�B'�B,B.B0!B33B5?B6FB7LB8RB:^B>wB@�BB�BB�BC�BD�BF�BH�BH�BJ�BJ�BK�BL�BM�BN�BP�BR�BT�BYB_;BcTBe`BgmBiyBk�Bm�Bp�Bs�B{�B~�B� B�B�B�1B�JB�VB�VB�VB�\B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�!B�'B�-B�3B�3B�3B�9B�FB�RB�^B�jB��B��B��B��B��B��B��B��B�
B�)B�TB�ZB�ZB�fB�sB�B�B�B��B��B��B��B��B	  B	DB	hB	oB	uB	{B	{B	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	"�B	$�B	%�B	)�B	-B	.B	0!B	49B	@�B	I�B	J�B	K�B	L�B	P�B	VB	XB	YB	[#B	[#B	\)B	[#B	\)B	]/B	^5B	^5B	^5B	_;B	`BB	`BB	aHB	bNB	bNB	cTB	dZB	e`B	gmB	gmB	hsB	jB	m�B	o�B	p�B	q�B	r�B	r�B	u�B	w�B	w�B	x�B	y�B	y�B	z�B	z�B	{�B	|�B	~�B	�B	�B	�1B	�DB	�VB	�hB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=y�#=q��=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`BPRES            TEMP            PSAL            PRES_ADJUSTED = PRES - (REPORTED_SURFACE_PRESSURE) for PADJ                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            PSAL_ADJ corrects CTD thermal lag (CTL) viz. Johnson et al, 2007, JAOT, effects of pressure adjustments, and PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment term r.                  PADJ REPORTED_SURFACE_PRESSURE =0.07 dbar                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CTL alpha = 0.021 & tau = 21 s with error equal to |correction| and for OW r = 0.9998 (+/-0.0014), vertically averaged dS = -0.007 (+/-0.055)                                                                                                                   Pressures adjusted using reported surface pressure when warranted.                                                                                                                                                                                              none                                                                                                                                                                                                                                                            After pressure and cell thermal lag correction of salinity values, OW correction estimated using mapping scales of 8 & 4 long. and 4 & 2 lat., no PV constraint, and decorrelation time scale of 10 years.                                                      202307212300292023072123002920230721230029  AO  ARCAADJP                                                                    20201125130037    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20201125130037  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20201125130037  QCF$                G�O�G�O�G�O�0               PM  ARSQPADJV1.1                                                                20230712105808  QC  PRES            @���D�9�G�O�                PM  ARSQCTM V1.1                                                                20230712105808  QC  PSAL            @���D�9�G�O�                PM  ARSQCOWGV1.1CTD_2021v2 + Argo_2021v03                                       20230721230929  IP                  G�O�G�O�G�O�                