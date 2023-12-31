CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  p   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2021-01-25T23:14:23Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
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
resolution        >�EȠ      
_FillValue        A.�~       axis      T           80   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    88   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
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
resolution        =���   axis      Z        	�  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 p  C0   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     	�  E�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 p  O`   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     	�  Q�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     	�  [�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 p  eP   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     	�  g�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 p  q�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     	�  s�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     	�  }�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 p  �p   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     	�  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 p  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     	�  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �    SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �    SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �    SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �    HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �,   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �0   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �4   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �8   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �<   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �|   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20210125231423  20210125231423  5903863 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               A   AO  4298                            2B  A   NAVIS_A                         0127                            120111                          863 @�G��Ֆ�1   @�G��Ֆ�@5p ě���db=p��
8   GPS     Primary sampling: mixed [deep: discrete, shallow: averaged]                                                                                                                                                                                                        A   A   A   @�33@�  A   A   A@  A`  A�  A�  A���A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� DxfDx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@qG�@�p�@�p�A�RA:�RAZ�RAz�RA�\)A�(�A�\)A�\)A�\)A�\)A�\)A�\)B�B�B�B�B&�B.�B6�B>�BF�BN�BV�B^�Bf�Bn�Bv�B~�B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�#�B�W
B�W
B�W
B�W
B�W
C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D j�D ��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��D	j�D	��D
j�D
��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��Dj�D��D j�D ��D!j�D!��D"j�D"��D#j�D#��D$j�D$��D%j�D%��D&j�D&��D'j�D'��D(j�D(��D)j�D)��D*j�D*��D+j�D+��D,j�D,��D-j�D-��D.j�D.��D/j�D/��D0j�D0��D1j�D1��D2j�D2��D3j�D3��D4j�D4��D5j�D5��D6j�D6��D7j�D7��D8j�D8��D9j�D9��D:j�D:��D;j�D;��D<j�D<��D=j�D=��D>j�D>��D?j�D?��D@j�D@��DAj�DA��DBj�DB��DCj�DC��DDj�DD��DEj�DE��DFj�DF��DGj�DG��DHj�DH��DIj�DI��DJj�DJ��DKj�DK��DLj�DL��DMj�DM��DNj�DN��DOj�DO��DPj�DP��DQj�DQ��DRj�DR��DSj�DS��DTj�DT��DUj�DU��DVj�DV��DWj�DW��DXj�DX��DYj�DY��DZj�DZ��D[j�D[��D\j�D\��D]j�D]��D^j�D^��D_j�D_��D`j�D`��Daj�Da��Dbj�Db��Dcj�Dc��Ddj�Dd��Dej�De��Dfj�Df��Dgj�Dg��Dhj�Dh��Dij�Di��Djj�Dj��Dkj�Dk��Dlj�Dl��Dmj�Dm��Dnj�Dn��Doj�Do��Dpj�Dp��Dqj�Dq��Drj�Dr��Dsj�Ds��Dtj�Dt��Duj�Du��Dvj�Dv��Dwj�Dw�GDxj�Dx��Dyj�Dy��Dzj�Dz��D{j�D{��D|j�D|��D}j�D}��D~j�D~��Dj�D��D�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD���D�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�8�D�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5qD�uqD��qD��qD�5q111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�\)A�\)A�^5A�`BA�`BA�bNA�bNA�dZA�hsA�hsA�l�A�t�A�v�A�\)A�A�A�=qA�VA�jA�AΩ�A��A���A���A��A�7LA�JA�S�A�+A�VA�&�A�A�t�A��A��uA��-A��#A�dZA���A�v�A�M�A�+A�ƨA�JA��A�1'A�ƨA�=qA��A�VA��mA��^A�p�A�%A��!A��A��A�ĜA�^5A�\)A���A��PA�ffA�JA��
A� �A��A�I�A�z�A��A�;dA��9A��A�G�A�bA��RA�5?A���A��A�/A��PA�?}A�K�A��`A��A�`BA��hA�ffA�/A�G�A�jA�(�A�O�A�/A�A�JA���A�7LA��\A�I�A�l�A���A��\A�z�A�ZA��
A�5?A�1A���A�1'A��FA�oA��+A��yA��uA��A~�A|=qAz�\Ax�Av�At��AsdZAr9XAq33AnffAl�jAi��Ag�Ag��Ag/Ae33Ab-A`��A_��A^�A]�;A]��A\��AZ��AY�7AW�TAUƨAS�AQ��AO�AL��AJ��AG�AE\)AC�AB�!AB �A@�/A?S�A>�DA=��A=`BA<�DA;"�A:n�A:$�A9�A9�A9C�A8�!A8JA7p�A5&�A3��A3dZA2ȴA0�jA/�;A/G�A.�A.�A.�A-��A,�A*�A*-A)33A(E�A'�PA&�uA%�TA%;dA$�A$JA"�A �\A ZA~�A��A�uA|�AI�AoA�HA�+A�A�A�mAbA7LA��AO�A��AbNA{A�A�wA�A-A��A`BA
jA	G�A	
=A��AC�A�RAA�A��A
=A/A�^A�A/A �`A ��@�33@�Q�@���@�dZ@�x�@�o@��@�1@�@� �@���@�P@���@��
@�dZ@�ff@�{@�hs@�@�`B@ߥ�@�n�@݁@�Q�@۝�@�-@أ�@�/@��/@�bN@ҸR@�{@Ѓ@ϝ�@��@ΰ!@���@͙�@�z�@�ff@�G�@��`@ȼj@�1'@�\)@Ɨ�@ŉ7@�?}@���@�ȴ@�@�%@�Z@�1@��w@�S�@�~�@��#@�hs@��@���@��@��H@��@�@��7@���@�1'@�C�@�=q@��h@�%@��`@���@�E�@���@��h@�G�@�bN@��@�S�@��R@�$�@��7@���@��@��
@�K�@�@�X@�X@�ƨ@���@��@���@��^@�hs@���@�9X@��F@�+@���@���@���@��+@�$�@�X@��@�Q�@��w@�K�@�dZ@���@�l�@�dZ@�S�@�;d@�v�@��@�X@�b@�33@��h@���@���@���@��@�G�@��@�M�@��!@�@�X@�V@��@���@�hs@���@�&�@��@���@��D@�r�@�z�@�r�@�r�@�z�@��@��@���@�t�@�\)@�;d@���@��@��@��H@��R@���@��+@�^5@�5?@�J@��#@�p�@�7L@�7L@�/@�%@���@�V@�%@���@�Ĝ@��9@���@��D@�j@�I�@�A�@�1'@� �@�b@�b@�  @��
@��P@�|�@�dZ@�S�@�"�@��y@��@���@��@���@���@�n�@�V@�-@�{@��@��^@���@���@�X@�%@��9@��j@��9@��D@�Z@�I�@�9X@��@�  @��
@��@�C�@��@��@���@�n�@�=q@�J@��@�@�p�@�O�@�V@��@�Ĝ@��j@��@�I�@�  @���@��P@�l�@�S�@�;d@��@���@�E�@��-@�p�@�X@��@�%@���@��@���@�I�@� �@�  @��m@��;@��w@�t�@�33@��@��\@�^5@�5?@�{@��T@���@�hs@�&�@�V@��/@��D@�r�@�Q�@�1'@�(�@�b@�b@�  @���@���@�\)@��@��\@�ff@�=q@�$�@�@��^@�p�@�?}@�/@�&�@�&�@��@�%@��@��/@���@��u@��@�r�@�b@��@+@~�y@~ȴ@~��@~5?@}�T@}�h@}�@|��@|�D@|Z@{�m@{�F@{��@{��@{�@{t�@{dZ@{33@z�H@z�!@z-@y��@y��@xĜ@xbN@x1'@x  @wK�@v�@vȴ@v��@v��@vv�@vE�@u�T@u��@t��@t(�@sS�@s"�@r��@r�\@rM�@r�@q��@q��@p�`@p �@p  @o�w@o;d@n��@n��@n5?@m�h@m�@l�/@lZ@k�m@kt�@k"�@j�\@jJ@h��@h�9@h��@hr�@hb@g�w@g��@g�P@g�P@g|�@gl�@f�y@f��@fv�@f@e�h@e?}@e/@d��@d(�@c�m@cƨ@cS�@b�H@b�H111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�\)A�\)A�^5A�`BA�`BA�bNA�bNA�dZA�hsA�hsA�l�A�t�A�v�A�\)A�A�A�=qA�VA�jA�AΩ�A��A���A���A��A�7LA�JA�S�A�+A�VA�&�A�A�t�A��A��uA��-A��#A�dZA���A�v�A�M�A�+A�ƨA�JA��A�1'A�ƨA�=qA��A�VA��mA��^A�p�A�%A��!A��A��A�ĜA�^5A�\)A���A��PA�ffA�JA��
A� �A��A�I�A�z�A��A�;dA��9A��A�G�A�bA��RA�5?A���A��A�/A��PA�?}A�K�A��`A��A�`BA��hA�ffA�/A�G�A�jA�(�A�O�A�/A�A�JA���A�7LA��\A�I�A�l�A���A��\A�z�A�ZA��
A�5?A�1A���A�1'A��FA�oA��+A��yA��uA��A~�A|=qAz�\Ax�Av�At��AsdZAr9XAq33AnffAl�jAi��Ag�Ag��Ag/Ae33Ab-A`��A_��A^�A]�;A]��A\��AZ��AY�7AW�TAUƨAS�AQ��AO�AL��AJ��AG�AE\)AC�AB�!AB �A@�/A?S�A>�DA=��A=`BA<�DA;"�A:n�A:$�A9�A9�A9C�A8�!A8JA7p�A5&�A3��A3dZA2ȴA0�jA/�;A/G�A.�A.�A.�A-��A,�A*�A*-A)33A(E�A'�PA&�uA%�TA%;dA$�A$JA"�A �\A ZA~�A��A�uA|�AI�AoA�HA�+A�A�A�mAbA7LA��AO�A��AbNA{A�A�wA�A-A��A`BA
jA	G�A	
=A��AC�A�RAA�A��A
=A/A�^A�A/A �`A ��@�33@�Q�@���@�dZ@�x�@�o@��@�1@�@� �@���@�P@���@��
@�dZ@�ff@�{@�hs@�@�`B@ߥ�@�n�@݁@�Q�@۝�@�-@أ�@�/@��/@�bN@ҸR@�{@Ѓ@ϝ�@��@ΰ!@���@͙�@�z�@�ff@�G�@��`@ȼj@�1'@�\)@Ɨ�@ŉ7@�?}@���@�ȴ@�@�%@�Z@�1@��w@�S�@�~�@��#@�hs@��@���@��@��H@��@�@��7@���@�1'@�C�@�=q@��h@�%@��`@���@�E�@���@��h@�G�@�bN@��@�S�@��R@�$�@��7@���@��@��
@�K�@�@�X@�X@�ƨ@���@��@���@��^@�hs@���@�9X@��F@�+@���@���@���@��+@�$�@�X@��@�Q�@��w@�K�@�dZ@���@�l�@�dZ@�S�@�;d@�v�@��@�X@�b@�33@��h@���@���@���@��@�G�@��@�M�@��!@�@�X@�V@��@���@�hs@���@�&�@��@���@��D@�r�@�z�@�r�@�r�@�z�@��@��@���@�t�@�\)@�;d@���@��@��@��H@��R@���@��+@�^5@�5?@�J@��#@�p�@�7L@�7L@�/@�%@���@�V@�%@���@�Ĝ@��9@���@��D@�j@�I�@�A�@�1'@� �@�b@�b@�  @��
@��P@�|�@�dZ@�S�@�"�@��y@��@���@��@���@���@�n�@�V@�-@�{@��@��^@���@���@�X@�%@��9@��j@��9@��D@�Z@�I�@�9X@��@�  @��
@��@�C�@��@��@���@�n�@�=q@�J@��@�@�p�@�O�@�V@��@�Ĝ@��j@��@�I�@�  @���@��P@�l�@�S�@�;d@��@���@�E�@��-@�p�@�X@��@�%@���@��@���@�I�@� �@�  @��m@��;@��w@�t�@�33@��@��\@�^5@�5?@�{@��T@���@�hs@�&�@�V@��/@��D@�r�@�Q�@�1'@�(�@�b@�b@�  @���@���@�\)@��@��\@�ff@�=q@�$�@�@��^@�p�@�?}@�/@�&�@�&�@��@�%@��@��/@���@��u@��@�r�@�b@��@+@~�y@~ȴ@~��@~5?@}�T@}�h@}�@|��@|�D@|Z@{�m@{�F@{��@{��@{�@{t�@{dZ@{33@z�H@z�!@z-@y��@y��@xĜ@xbN@x1'@x  @wK�@v�@vȴ@v��@v��@vv�@vE�@u�T@u��@t��@t(�@sS�@s"�@r��@r�\@rM�@r�@q��@q��@p�`@p �@p  @o�w@o;d@n��@n��@n5?@m�h@m�@l�/@lZ@k�m@kt�@k"�@j�\@jJ@h��@h�9@h��@hr�@hb@g�w@g��@g�P@g�P@g|�@gl�@f�y@f��@fv�@f@e�h@e?}@e/@d��@d(�@c�m@cƨ@cS�@b�H@b�H111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BVBVBVBVBVBVBVBVBVBVBVBVBVBT�BS�BR�BS�BS�BM�BE�B,B�B�B&�B33B=qB>wB=qB<jB5?B0!B0!B/B,B)�B�B�B�B�B!�B"�B'�B+B)�B(�B(�B&�B$�B$�B$�B%�B%�B#�B�B�B�B�B�B�B�B�B�B�B�B\B%B  B��B�B�B��B��B��B�jB�LB��B��B~�BbNBK�B<jB$�B�B�B��B�ZB��B�}B�FB�B��B��B{�Bl�B^5BXBQ�BH�BB�B5?B�B1B
�B
�B
��B
��B
ǮB
��B
�^B
�3B
��B
�oB
�1B
v�B
l�B
cTB
S�B
G�B
<jB
-B
"�B
�B
bB
1B	��B	�B	�B	��B	��B	ɺB	�wB	�!B	��B	��B	��B	��B	��B	�bB	�%B	{�B	n�B	_;B	S�B	G�B	?}B	-B	%�B	�B	\B	+B	B	B��B��B��B��B��B��B�B�B�B�B�sB�fB�ZB�NB�;B�B��B��B��B��BɺBǮBƨBŢBÖB��B�}B�^B�LB�9B�!B�B��B��B��B��B��B��B�\B�hB�PB|�Bx�Bu�Bq�Bl�Bl�BjBgmBe`BaHB\)B[#B\)B[#BZBZBZBZBYBXBXBZBYBXBT�BS�BQ�BS�BS�BT�BT�BS�BR�BQ�BP�BO�BO�BM�BK�BI�BI�BG�BE�BD�BB�BA�B>wB;dB8RB6FB7LB7LB6FB6FB5?B49B49B5?B8RB9XB:^B;dB;dB<jB<jBB�BA�BA�BC�BC�BE�BF�BI�BM�BL�BL�BK�BM�B^5Be`BffBhsBiyBk�Bp�Bz�Bx�Bz�B�B�%B�7B�=B�DB�VB��B��B��B��B��B��B��B��B��B��B�B�!B�'B�'B�3B�3B�9B�FB�}BĜBƨBȴB��B��B��B��B��B��B��B��B�B�/B�;B�;B�BB�;B�5B�BB�ZB�B�B�B��B��B��B��B	
=B	VB	\B	bB	\B	hB	oB	�B	�B	�B	 �B	"�B	$�B	&�B	(�B	+B	)�B	(�B	&�B	&�B	$�B	#�B	%�B	)�B	-B	49B	6FB	>wB	C�B	D�B	C�B	D�B	F�B	H�B	N�B	VB	ZB	]/B	^5B	_;B	bNB	cTB	cTB	dZB	hsB	k�B	n�B	r�B	t�B	u�B	x�B	~�B	�B	�B	�B	�B	�B	�1B	�7B	�7B	�DB	�PB	�\B	�hB	�hB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�'B	�'B	�-B	�-B	�9B	�FB	�LB	�RB	�RB	�^B	�^B	�^B	�^B	�^B	�dB	�qB	�wB	��B	��B	ÖB	ĜB	ŢB	ŢB	ǮB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�#B	�#B	�)B	�/B	�/B	�5B	�;B	�;B	�HB	�HB	�NB	�TB	�TB	�ZB	�`B	�fB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
+B
+B
+B
1B
1B
	7B
	7B
	7B
	7B

=B

=B

=B
DB
DB
DB
JB
JB
PB
PB
PB
PB
PB
PB
PB
PB
PB
VB
VB
VB
bB
bB
bB
bB
hB
oB
oB
oB
oB
oB
oB
uB
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
 �B
 �B
 �B
 �B
!�B
!�B
!�B
!�B
"�B
"�B
"�B
#�B
#�B
#�B
#�B
$�B
$�B
$�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111BVBVBVBVBVBVBVBVBVBVBVBVBVBT�BS�BR�BS�BS�BM�BE�B,B�B�B&�B33B=qB>wB=qB<jB5?B0!B0!B/B,B)�B�B�B�B�B!�B"�B'�B+B)�B(�B(�B&�B$�B$�B$�B%�B%�B#�B�B�B�B�B�B�B�B�B�B�B�B\B%B  B��B�B�B��B��B��B�jB�LB��B��B~�BbNBK�B<jB$�B�B�B��B�ZB��B�}B�FB�B��B��B{�Bl�B^5BXBQ�BH�BB�B5?B�B1B
�B
�B
��B
��B
ǮB
��B
�^B
�3B
��B
�oB
�1B
v�B
l�B
cTB
S�B
G�B
<jB
-B
"�B
�B
bB
1B	��B	�B	�B	��B	��B	ɺB	�wB	�!B	��B	��B	��B	��B	��B	�bB	�%B	{�B	n�B	_;B	S�B	G�B	?}B	-B	%�B	�B	\B	+B	B	B��B��B��B��B��B��B�B�B�B�B�sB�fB�ZB�NB�;B�B��B��B��B��BɺBǮBƨBŢBÖB��B�}B�^B�LB�9B�!B�B��B��B��B��B��B��B�\B�hB�PB|�Bx�Bu�Bq�Bl�Bl�BjBgmBe`BaHB\)B[#B\)B[#BZBZBZBZBYBXBXBZBYBXBT�BS�BQ�BS�BS�BT�BT�BS�BR�BQ�BP�BO�BO�BM�BK�BI�BI�BG�BE�BD�BB�BA�B>wB;dB8RB6FB7LB7LB6FB6FB5?B49B49B5?B8RB9XB:^B;dB;dB<jB<jBB�BA�BA�BC�BC�BE�BF�BI�BM�BL�BL�BK�BM�B^5Be`BffBhsBiyBk�Bp�Bz�Bx�Bz�B�B�%B�7B�=B�DB�VB��B��B��B��B��B��B��B��B��B��B�B�!B�'B�'B�3B�3B�9B�FB�}BĜBƨBȴB��B��B��B��B��B��B��B��B�B�/B�;B�;B�BB�;B�5B�BB�ZB�B�B�B��B��B��B��B	
=B	VB	\B	bB	\B	hB	oB	�B	�B	�B	 �B	"�B	$�B	&�B	(�B	+B	)�B	(�B	&�B	&�B	$�B	#�B	%�B	)�B	-B	49B	6FB	>wB	C�B	D�B	C�B	D�B	F�B	H�B	N�B	VB	ZB	]/B	^5B	_;B	bNB	cTB	cTB	dZB	hsB	k�B	n�B	r�B	t�B	u�B	x�B	~�B	�B	�B	�B	�B	�B	�1B	�7B	�7B	�DB	�PB	�\B	�hB	�hB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�'B	�'B	�-B	�-B	�9B	�FB	�LB	�RB	�RB	�^B	�^B	�^B	�^B	�^B	�dB	�qB	�wB	��B	��B	ÖB	ĜB	ŢB	ŢB	ǮB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�#B	�#B	�)B	�/B	�/B	�5B	�;B	�;B	�HB	�HB	�NB	�TB	�TB	�ZB	�`B	�fB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
+B
+B
+B
1B
1B
	7B
	7B
	7B
	7B

=B

=B

=B
DB
DB
DB
JB
JB
PB
PB
PB
PB
PB
PB
PB
PB
PB
VB
VB
VB
bB
bB
bB
bB
hB
oB
oB
oB
oB
oB
oB
uB
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
 �B
 �B
 �B
 �B
!�B
!�B
!�B
!�B
"�B
"�B
"�B
#�B
#�B
#�B
#�B
$�B
$�B
$�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.33 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20210125231423                              AO  ARCAADJP                                                                    20210125231423    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20210125231423  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20210125231423  QCF$                G�O�G�O�G�O�0               