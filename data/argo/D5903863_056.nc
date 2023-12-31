CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       d2014-07-21T23:39:19Z creation; 2014-07-21T23:39:19Z updated; 2015-09-28T12:13:27Z converted from 3.0   
references        (http://www.argodatamgt.org/Documentation   comment           user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7    PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7`   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8    PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8$   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8D   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8d   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�E�vQ�   
_FillValue        A.�~       axis      T           8h   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8p   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�E�vQ�   
_FillValue        A.�~            8t   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8|   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ID   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M,   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  `�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  pL   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �l   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �T   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �t   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ݔ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �    HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �@   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �P   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �T   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �d   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �h   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �l   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �pArgo profile    3.1 1.2 19500101000000  20140721233919  20170523133337  5903863 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               8A   AO  4298_0127_056                   2C  D   NAVIS_A                         0127                            120111                          863 @֫�|5 
1   @֫�,_�@5����m�d��\(��1   GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                      8A   A   A   @���@�  A   A   AA��A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
�C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D fD � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ D�|�D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ Dݼ�D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @s�@�Q�@�Q�A(�A9AX(�Ax(�A�{A�{A�{A�{A�{A�{A�{A�{B
=B
=B
=B
=B&
=B.
=B6
=B>
=BF
=BN
=BV
=B^
=Bf
=Bn
=Bv
=B~
=B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�C��C��C��C��C	�)C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��C��HC��HCִ{C��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��D `�D �D`�D�D`�D�D`�D�D`�D�D`�D�D`�D�D`�D�D`�D�D	`�D	�D
`�D
�D`�D�D`�D�D`�D�D`�D�D`�D�D`�D�D`�D�D`�D�D`�D�D`�D�D`�D�D`�D�D`�D�D`�D�D`�D�D`�D�D`�D�D`�D�D`�D�D`�D�D`�D�D `�D �D!`�D!�D"`�D"�D#`�D#�D$`�D$�D%`�D%�D&`�D&�D'`�D'�D(`�D(�D)`�D)�D*`�D*�D+`�D+�D,`�D,�D-`�D-�D.`�D.�D/`�D/�D0`�D0�D1`�D1�D2`�D2�D3`�D3�D4`�D4�D5`�D5�D6`�D6�D7`�D7�D8`�D8�D9`�D9�D:`�D:�D;`�D;�D<`�D<�D=`�D=�D>`�D>�D?`�D?�D@`�D@�DA`�DA�DB`�DB�DC`�DC�DD`�DD�DE`�DE�DF`�DF�DG`�DG�DH`�DH�DI`�DI�DJ`�DJ�DK`�DK�DL`�DL�DM`�DM�DN`�DN�DO`�DO�DP`�DP�DQ`�DQ�DR`�DR�DS`�DS�DT`�DT�DU`�DU�DV`�DV�DW`�DW�DX`�DX�DY`�DY�DZ`�DZ�D[`�D[�D\`�D\�D]`�D]�D^`�D^�D_`�D_�D``�D`�Da`�Da�Db`�Db�Dc`�Dc�Dd`�Dd�De`�De�Df`�Df�Dg`�Dg�Dh`�Dh�Di`�Di�Dj`�Dj�Dk`�Dk�Dl`�Dl�Dm`�Dm�Dn`�Dn�Do`�Do�Dp`�Dp�Dq`�Dq�Dr`�Dr�Ds`�Ds�Dt`�Dt�Du`�Du�Dv`�Dv�Dw`�Dw�Dx`�Dx�Dy`�Dy�Dz`�Dz�D{`�D{�D|`�D|�D}`�D}�D~`�D~�D`�D�D�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD���D��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD°RD��RD�0RD�pRDðRD��RD�0RD�pRDİRD��RD�0RD�pRDŰRD��RD�0RD�pRDưRD��RD�0RD�pRDǰRD��RD�0RD�pRDȰRD��RD�0RD�pRDɰRD��RD�0RD�pRDʰRD��RD�0RD�pRD˰RD��RD�0RD�pRD̰RD��RD�0RD�pRDͰRD��RD�0RD�pRDΰRD��RD�0RD�pRDϰRD��RD�0RD�pRDаRD��RD�0RD�pRDѰRD��RD�0RD�mDҰRD��RD�0RD�pRDӰRD��RD�0RD�pRD԰RD��RD�0RD�pRDհRD��RD�0RD�pRDְRD��RD�0RD�pRDװRD��RD�0RD�pRDذRD��RD�0RD�pRDٰRD��RD�0RD�pRDڰRD��RD�0RD�pRD۰RD��RD�0RD�pRDܰRD��RD�0RD�pRDݭD��RD�0RD�pRDްRD��RD�0RD�pRD߰RD��RD�0RD�pRD�RD��RD�0RD�pRD�RD��RD�0RD�pRD�RD��RD�0RD�pRD�RD��RD�0RD�pRD�RD��RD�0RD�pRD�RD��RD�0RD�pRD�RD��RD�0RD�pRD�RD��RD�0RD�pRD�RD��RD�0RD�pRD�RD��RD�0RD�pRD�RD��RD�0RD�pRD�RD��RD�0RD�pRD�RD��RD�0RD�pRD��RD��RD�0RD�pRD�RD��RD�0RD�pRD�RD��RD�0RD�pRD�RD��RD�0RD�pRD�RD��RD�0RD�pRD�RD��RD�0RD�pRD�RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��Aװ!Aװ!Aװ!Aײ-A״9A׶FA״9A׺^A׸RA׶FA׶FAק�A׋DA�VA�33A�A�|�AՍPAӇ+A�ĜAЗ�A�I�A�ffA�O�A��/A���AƗ�Aĝ�A�1A¾wA�7LA��A�VA��TA�`BA�1A���A��HA�;dA��A���A��A��TA�v�A���A���A��A���A�bA��A���A��A��A���A�dZA�=qA�"�A��A��7A�{A���A�^5A��A�ȴA�XA���A��A���A�x�A���A��hA���A�`BA���A�
=A�  A�33A��FA��
A���A�33A��yA�l�A���A�|�A�A�A��DA�jA�O�A�x�A�VA��DA���A���A�oA�&�A�33A�VA��RA��/A�K�A��A��A��A�E�A�O�A��A���A��RA��A�v�A���A��9A�9XA�\)A�M�A~�!A}�^A|�Az9XAx�\Awp�Av �Au&�Ar=qAn$�Aj�DAh��Ag��Af9XAcƨAa�hA_%A[;dAY/AX�AXA�AV^5AU%AR �AO��AO33AN��AN(�AK��AKAJ(�AH��AF-AC��AB��ABbNA@��A@�A@�A?33A=S�A<^5A<1A;?}A: �A9�#A8�!A7�#A7XA6�A5�A3��A2�A1�A0�9A.�A-��A,��A,E�A+C�A*n�A)\)A(bNA'��A'��A&�`A&�\A&�A%�wA%\)A$�DA$ffA#�
A"r�A�A�A��A�A��A�A�HA1A"�AVA�TAȴAA�mA�FAG�At�AbA�A��A��A��AA	��A��A1A�hAM�AVAĜAI�A��AC�A��Ax�A �yA ��A -@��P@��@���@�=q@���@�I�@��@�A�@���@���@�C�@�R@�`B@�b@�;d@���@�%@�1@�t�@�
=@�J@�V@�w@�$�@�G�@�D@�9X@���@�@�@���@�ƨ@�~�@�G�@�A�@���@�I�@�v�@с@�Ĝ@��@�v�@̋D@��y@�5?@Ɂ@��@�o@�b@�J@�b@��y@���@�bN@�C�@��@��`@�1@��@�5?@���@�p�@���@�ƨ@���@���@�|�@��@��@�dZ@�+@��H@���@�-@���@�Z@�^5@�b@�t�@��@��@�hs@�?}@���@��D@�|�@��H@�{@�x�@�A�@��m@��@��R@�^5@��@��@��@�C�@�"�@�o@��R@�V@���@�p�@�/@��@�bN@�1@��@�r�@��j@��j@���@��u@�Q�@��@�z�@��m@�K�@��!@�ȴ@��T@���@��`@���@�(�@�o@�@���@�@��@��@�
=@�33@�V@��^@��^@��-@���@�X@��`@�9X@��m@��F@�t�@�l�@�33@��R@�ff@�v�@��!@��H@��\@�ff@�5?@��@��@�@��#@��7@���@�x�@�p�@��@��D@�j@�I�@�1@�  @���@�  @�1@��
@�+@���@��!@��!@��!@��+@�-@�@�`B@�7L@���@���@��/@�z�@�A�@�r�@��D@�r�@�I�@�bN@�Z@�Z@��;@��w@���@���@�|�@�K�@��@��+@�-@�$�@��@�{@��@�p�@�X@�&�@��@�r�@�A�@���@���@�t�@�\)@�K�@�C�@�C�@�;d@�+@���@���@���@�5?@��@��^@���@��@�hs@�hs@�p�@�G�@��@�%@���@���@���@���@�Ĝ@�Ĝ@��j@��9@���@��u@��@��j@��u@�(�@��@���@�t�@�dZ@�S�@�33@�o@��@��H@��@��+@�-@�$�@��@�$�@�$�@�$�@�-@�-@�-@�-@�$�@�$�@�@��@��T@��T@���@��^@��^@��7@�%@��@
=@~�y@~ȴ@}`B@}O�@|�j@|(�@|I�@|�@|��@|z�@|�@|�@|�@|1@{�
@{t�@z�!@zM�@y�#@yx�@y7L@y%@x�u@wl�@v�y@vv�@v5?@u�@u��@u@u�-@u��@u��@u�h@u`B@u/@t�D@s�
@r�\@r=q@q��@q%@pQ�@p1'@p1'@p �@o�@o�P@ol�@o�@nȴ@n��@n5?@m�T@mp�@l��@lZ@k�
@kt�@j�@j�\@jM�@jJ@i�@ix�@i%@h��@hĜ@hr�@hQ�@h1'@hb@g��@g|�@f��@f��@fff@f5?@f@e@e�h@ep�@eO�@e?}@e�@d��@d��@d�@d�@d�D@dZ@d9X@c�m@c�
@c�F@c��@cdZ@c33@c@b�H@b��@b��@b^5@b=q@b-@a�#@ahs@a�@`��@`��@`�u@`Q�@`1'@`1'@` �@_�@_�;@_�w@_l�@_l�@_;d@^�@^v�@^@]�@]p�@]`B@]O�@\�@\j@\9X@\1@[�
@[��@Z�@Z�H@Z��@Z��@Z~�@Z-@Y�#@YX@XA�@W\)@W�@V�y@V�+@V@U�T@U@U�-@T�/@S�m@S��@SS�@S"�@R�H@R�\@Rn�@RM�@Qx�@P��@O�w@O
=@N��@M�T@M��@M�@Mp�@M?}@L�@L�j@L��@Lz�@K�F@K33@K@J��@J��@J�\@JM�@Ix�@I&�@I�@I�@H��@H��@H1'@G�w@G+@F�R@F��@Fv�@FV@F5?@E�T@E�@E/@D��@DZ@D9X@D�@D1@C��@C�m@C��@C33@Co@C@B�@Bn�@B-@A�#@A��@A�7@Ax�@AX@A�@A%@@�`@@��@@��@?�@?l�@?;d@?
=@>v�@>@=�h@=�@<�@;�@;33@:�H@:�!@:~�@:~�@:^5@:=q@:-@:�@:J@9�^@9�7@8��@8b@7�@7��@7�P@7|�@7l�@7+@6ȴ@6ff@5�@5�@5`B@5?}@5�@4��@4��@4�@4�@3dZ@3o@2�H@2�!@2��@2M�@1�#@1��@1�7@1X@1�@1�@1%@0��@0r�@/�;@/��@/
=@.��@.�+@.5?@.@-�T@-��@-�-@-�@-p�@-p�@-`B@-?}@-V@,��@,9X@+ƨ@+33@+@*�@*�H@*��@*��@*M�@)��@)��@)&�@(�`@(�@(b@'�;@'��@'�@'|�@'\)@';d@&��@&��@&�+@&�+@&v�@&ff@&ff@&V@&$�@%�T@%��@%O�@%V@$��@$�@$��@$�D@$j@$I�@$1@#�
@#ƨ@#��@#dZ@#o@"�!@"M�@"M�@"M�@"�@"�@"�@"�@"J@"J@"J@!�@!�7@!X@!G�@!G�@!G�@!G�@!&�@ ��@ ��@ Ĝ@ �9@ �u@ r�@ bN@ Q�@ 1'@ b@�@�@�P@l�@K�@\)@;d@��@�R@��@ff@5?@$�@{@{@@�h@p�@`B@?}@�/@I�@�m@��@t�@33@o@�@�H@��@n�@J@��@��@�7@hs@7L@��@�@bN@A�@�w@
=@ȴ@E�@5?@$�@{@{@�@�T@@�h@�@`B@��@�@�@�@�/@�j@j@I�@(�@��@�m@ƨ@��@��@��@S�@@��@��@~�@M�@=q@�@�#@��@�7@x�@G�@&�@Ĝ@�9@�9@��@�@1'@  @�;@�w@l�@l�@\)@+@
=@�y@�y@ȴ@��@v�@V@5?@5?@$�@$�@$�@{@�T@��@��@�-@�h@�h111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 Aװ!Aװ!Aװ!Aײ-A״9A׶FA״9A׺^A׸RA׶FA׶FAק�A׋DA�VA�33A�A�|�AՍPAӇ+A�ĜAЗ�A�I�A�ffA�O�A��/A���AƗ�Aĝ�A�1A¾wA�7LA��A�VA��TA�`BA�1A���A��HA�;dA��A���A��A��TA�v�A���A���A��A���A�bA��A���A��A��A���A�dZA�=qA�"�A��A��7A�{A���A�^5A��A�ȴA�XA���A��A���A�x�A���A��hA���A�`BA���A�
=A�  A�33A��FA��
A���A�33A��yA�l�A���A�|�A�A�A��DA�jA�O�A�x�A�VA��DA���A���A�oA�&�A�33A�VA��RA��/A�K�A��A��A��A�E�A�O�A��A���A��RA��A�v�A���A��9A�9XA�\)A�M�A~�!A}�^A|�Az9XAx�\Awp�Av �Au&�Ar=qAn$�Aj�DAh��Ag��Af9XAcƨAa�hA_%A[;dAY/AX�AXA�AV^5AU%AR �AO��AO33AN��AN(�AK��AKAJ(�AH��AF-AC��AB��ABbNA@��A@�A@�A?33A=S�A<^5A<1A;?}A: �A9�#A8�!A7�#A7XA6�A5�A3��A2�A1�A0�9A.�A-��A,��A,E�A+C�A*n�A)\)A(bNA'��A'��A&�`A&�\A&�A%�wA%\)A$�DA$ffA#�
A"r�A�A�A��A�A��A�A�HA1A"�AVA�TAȴAA�mA�FAG�At�AbA�A��A��A��AA	��A��A1A�hAM�AVAĜAI�A��AC�A��Ax�A �yA ��A -@��P@��@���@�=q@���@�I�@��@�A�@���@���@�C�@�R@�`B@�b@�;d@���@�%@�1@�t�@�
=@�J@�V@�w@�$�@�G�@�D@�9X@���@�@�@���@�ƨ@�~�@�G�@�A�@���@�I�@�v�@с@�Ĝ@��@�v�@̋D@��y@�5?@Ɂ@��@�o@�b@�J@�b@��y@���@�bN@�C�@��@��`@�1@��@�5?@���@�p�@���@�ƨ@���@���@�|�@��@��@�dZ@�+@��H@���@�-@���@�Z@�^5@�b@�t�@��@��@�hs@�?}@���@��D@�|�@��H@�{@�x�@�A�@��m@��@��R@�^5@��@��@��@�C�@�"�@�o@��R@�V@���@�p�@�/@��@�bN@�1@��@�r�@��j@��j@���@��u@�Q�@��@�z�@��m@�K�@��!@�ȴ@��T@���@��`@���@�(�@�o@�@���@�@��@��@�
=@�33@�V@��^@��^@��-@���@�X@��`@�9X@��m@��F@�t�@�l�@�33@��R@�ff@�v�@��!@��H@��\@�ff@�5?@��@��@�@��#@��7@���@�x�@�p�@��@��D@�j@�I�@�1@�  @���@�  @�1@��
@�+@���@��!@��!@��!@��+@�-@�@�`B@�7L@���@���@��/@�z�@�A�@�r�@��D@�r�@�I�@�bN@�Z@�Z@��;@��w@���@���@�|�@�K�@��@��+@�-@�$�@��@�{@��@�p�@�X@�&�@��@�r�@�A�@���@���@�t�@�\)@�K�@�C�@�C�@�;d@�+@���@���@���@�5?@��@��^@���@��@�hs@�hs@�p�@�G�@��@�%@���@���@���@���@�Ĝ@�Ĝ@��j@��9@���@��u@��@��j@��u@�(�@��@���@�t�@�dZ@�S�@�33@�o@��@��H@��@��+@�-@�$�@��@�$�@�$�@�$�@�-@�-@�-@�-@�$�@�$�@�@��@��T@��T@���@��^@��^@��7@�%@��@
=@~�y@~ȴ@}`B@}O�@|�j@|(�@|I�@|�@|��@|z�@|�@|�@|�@|1@{�
@{t�@z�!@zM�@y�#@yx�@y7L@y%@x�u@wl�@v�y@vv�@v5?@u�@u��@u@u�-@u��@u��@u�h@u`B@u/@t�D@s�
@r�\@r=q@q��@q%@pQ�@p1'@p1'@p �@o�@o�P@ol�@o�@nȴ@n��@n5?@m�T@mp�@l��@lZ@k�
@kt�@j�@j�\@jM�@jJ@i�@ix�@i%@h��@hĜ@hr�@hQ�@h1'@hb@g��@g|�@f��@f��@fff@f5?@f@e@e�h@ep�@eO�@e?}@e�@d��@d��@d�@d�@d�D@dZ@d9X@c�m@c�
@c�F@c��@cdZ@c33@c@b�H@b��@b��@b^5@b=q@b-@a�#@ahs@a�@`��@`��@`�u@`Q�@`1'@`1'@` �@_�@_�;@_�w@_l�@_l�@_;d@^�@^v�@^@]�@]p�@]`B@]O�@\�@\j@\9X@\1@[�
@[��@Z�@Z�H@Z��@Z��@Z~�@Z-@Y�#@YX@XA�@W\)@W�@V�y@V�+@V@U�T@U@U�-@T�/@S�m@S��@SS�@S"�@R�H@R�\@Rn�@RM�@Qx�@P��@O�w@O
=@N��@M�T@M��@M�@Mp�@M?}@L�@L�j@L��@Lz�@K�F@K33@K@J��@J��@J�\@JM�@Ix�@I&�@I�@I�@H��@H��@H1'@G�w@G+@F�R@F��@Fv�@FV@F5?@E�T@E�@E/@D��@DZ@D9X@D�@D1@C��@C�m@C��@C33@Co@C@B�@Bn�@B-@A�#@A��@A�7@Ax�@AX@A�@A%@@�`@@��@@��@?�@?l�@?;d@?
=@>v�@>@=�h@=�@<�@;�@;33@:�H@:�!@:~�@:~�@:^5@:=q@:-@:�@:J@9�^@9�7@8��@8b@7�@7��@7�P@7|�@7l�@7+@6ȴ@6ff@5�@5�@5`B@5?}@5�@4��@4��@4�@4�@3dZ@3o@2�H@2�!@2��@2M�@1�#@1��@1�7@1X@1�@1�@1%@0��@0r�@/�;@/��@/
=@.��@.�+@.5?@.@-�T@-��@-�-@-�@-p�@-p�@-`B@-?}@-V@,��@,9X@+ƨ@+33@+@*�@*�H@*��@*��@*M�@)��@)��@)&�@(�`@(�@(b@'�;@'��@'�@'|�@'\)@';d@&��@&��@&�+@&�+@&v�@&ff@&ff@&V@&$�@%�T@%��@%O�@%V@$��@$�@$��@$�D@$j@$I�@$1@#�
@#ƨ@#��@#dZ@#o@"�!@"M�@"M�@"M�@"�@"�@"�@"�@"J@"J@"J@!�@!�7@!X@!G�@!G�@!G�@!G�@!&�@ ��@ ��@ Ĝ@ �9@ �u@ r�@ bN@ Q�@ 1'@ b@�@�@�P@l�@K�@\)@;d@��@�R@��@ff@5?@$�@{@{@@�h@p�@`B@?}@�/@I�@�m@��@t�@33@o@�@�H@��@n�@J@��@��@�7@hs@7L@��@�@bN@A�@�w@
=@ȴ@E�@5?@$�@{@{@�@�T@@�h@�@`B@��@�@�@�@�/@�j@j@I�@(�@��@�m@ƨ@��@��@��@S�@@��@��@~�@M�@=q@�@�#@��@�7@x�@G�@&�@Ĝ@�9@�9@��@�@1'@  @�;@�w@l�@l�@\)@+@
=@�y@�y@ȴ@��@v�@V@5?@5?@$�@$�@$�@{@�T@��@��@�-@�h@�h111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBH�BH�BH�BH�BH�BH�BH�BH�BH�BH�BH�BG�BE�BC�B@�B<jB0!B�B�B�B�B�B�B�B%�B �B.BVBS�BI�B@�BI�B@�B6FB0!B2-B8RB:^B8RB2-B.B&�B6FBD�BQ�Bt�Bu�B_;BP�BJ�BT�B?}BN�BK�BI�BG�BVBgmBiyBK�B-B'�B�BbB1B  B��B�fB�B��B��B��B�?B�3B�B��B��B�%Bx�Bl�B`BBM�BD�B;dB2-B�BbB��B�HB�B��BÖB�3B��B�DBp�B`BBR�BH�B;dB2-B+B�BVBB
�B
�B
ŢB
�XB
��B
��B
�PB
y�B
o�B
bNB
H�B
;dB
33B
)�B
�B
bB
+B	��B	��B	�ZB	ȴB	�-B	��B	��B	�hB	�B	r�B	cTB	L�B	@�B	<jB	8RB	/B	'�B	�B	bB	JB	
=B	B��B�B�B�`B�5B�B��B��B��B�B�B�5B�;B�/B�B�B�B��B��B��B��B��BǮBB�qB�XB�3B�B��B��B��B��B��B��B��B��B��B��B��B�{B�uB�hB�bB�\B�PB�7B�B�B}�B{�By�Bw�Bu�Bs�Bp�Bm�BjBiyBhsBgmBffBdZBbNB`BB_;B`BBbNBcTBe`BffBdZBcTBbNBcTBffBffBffBffBe`BdZBe`Be`BdZBdZBdZBdZBdZBdZBe`BffBffBiyBhsBgmBe`Be`BffBk�Bo�Bs�Bs�Bt�Bt�Bt�Bu�Bu�Bt�Bn�Bk�BhsBffBe`BgmBffB`BBT�BT�BT�BVBVBZB\)B]/B]/B]/B^5B_;B_;B^5B]/B[#BXBT�BS�BT�BT�BS�BVBXB[#B[#B\)B`BBbNBdZBe`BgmBm�Bp�Bt�Bw�B}�B~�B�B�VB�hB�uB�{B��B��B��B��B��B��B��B��B��B��B��B�B�B�-B�9B�FB�LB�LB�^B�wBBƨBǮBǮBǮBǮBȴB��B��B��B��B��B�B�B�NB�yB�B�B��B��B��B��B��B��B	B	DB	bB	oB	oB	oB	uB	uB	{B	�B	�B	�B	(�B	)�B	+B	.B	-B	/B	1'B	2-B	33B	49B	6FB	5?B	5?B	7LB	9XB	:^B	;dB	>wB	@�B	C�B	F�B	J�B	L�B	L�B	L�B	M�B	N�B	O�B	Q�B	T�B	XB	]/B	_;B	`BB	bNB	e`B	gmB	iyB	iyB	jB	k�B	l�B	n�B	p�B	q�B	r�B	r�B	q�B	r�B	r�B	t�B	w�B	z�B	}�B	}�B	� B	�B	�B	�1B	�=B	�DB	�JB	�\B	�\B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�-B	�-B	�9B	�?B	�?B	�?B	�?B	�FB	�FB	�FB	�RB	�^B	�jB	�}B	��B	ÖB	ŢB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�;B	�TB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
%B
+B
+B
+B
1B
1B
1B
	7B

=B

=B
DB
DB
DB
DB
PB
VB
VB
\B
bB
bB
bB
hB
hB
hB
hB
hB
hB
oB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
"�B
"�B
"�B
"�B
"�B
"�B
#�B
#�B
#�B
#�B
#�B
#�B
$�B
$�B
$�B
$�B
$�B
$�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
'�B
'�B
(�B
(�B
(�B
(�B
(�B
)�B
)�B
)�B
)�B
)�B
)�B
+B
+B
+B
+B
+B
+B
,B
,B
-B
.B
.B
.B
/B
/B
/B
/B
/B
0!B
1'B
1'B
1'B
2-B
2-B
2-B
2-B
2-B
33B
33B
49B
5?B
5?B
6FB
6FB
6FB
6FB
6FB
7LB
7LB
7LB
7LB
8RB
8RB
8RB
9XB
9XB
9XB
9XB
:^B
:^B
:^B
:^B
:^B
:^B
;dB
;dB
<jB
<jB
<jB
<jB
<jB
<jB
=qB
=qB
=qB
=qB
>wB
>wB
>wB
>wB
>wB
>wB
>wB
?}B
?}B
?}B
?}B
?}B
?}B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
A�B
B�B
B�B
B�B
B�B
C�B
C�B
C�B
D�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
S�B
S�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
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
W
B
XB
XB
XB
XB
YB
YB
XB
YB
YB
ZB
ZB
[#B
[#B
[#B
[#B
[#B
[#B
[#B
\)B
\)B
\)B
\)B
]/B
]/B
^5B
^5B
^5B
^5B
^5B
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
_;B
_;B
_;B
_;B
`BB
_;B
`BB
`BB
_;B
_;B
`BB
`BB
`BB
`BB
`BB
`BB
`BB
`BB
`BB
`BB
`BB
`BB
aHB
aHB
`BB
`BB
aHB
`BB
`BB
`BB
`BB
`BB
`BB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
cTB
cTB
cTB
cTB
dZB
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
ffB
ffB
ffB
ffB
ffB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
hsB
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
iyB
jB
jB
jB
jB
jB
jB
k�B
k�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
n�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BH�BH�BH�BH�BH�BH�BH�BH�BH�BH�BI	BH0BF�BD0BAIB><B30B&LBBB�B�B�B#�B)�B)�B&dB6�BYBZBQlBFBM�BB�B8�B1�B4\B;�B=}B:�B5"B1�B+�B8	BDHBQ:BvBx�BawBQ�BK�BW�B?BP�BL�BJ�BHGBVxBj:Bp]BQ�B/bB,KB&B�B
*B�B�nB�=BؒB�BϑBƅB��B��B��B�CB�>B�mB|BoIBd�BO�BFB=_B6�B �BmB,B�@B�!B�TB�B�aB��B��BuBdABU�BL�B=�B4B.�B#.BB?B
��B
��B
ɳB
�{B
��B
��B
��B
|MB
s�B
lB
M�B
>	B
6�B
0B
 .B
�B

�B
 "B	��B	��B	��B	�B	��B	�B	��B	��B	y�B	m	B	Q�B	A�B	=�B	<�B	2B	-�B	B	�B	�B		B	�B��B�B� B�7B��B��B��B��B��B�`B��B�B�&B�iBܩB�GB�B؎B״BҊB�{B�WB�hB��B�"B��B�9B�&B�B�B��B�:B��B�|B��B��B��B��B��B��B��B��B�B�GB�DB�)B�LB��B�{B~mB|�By�Bx3Bv?Bu�Bp�BmWBk�Bh�Bh&Bg�BiBfBa�Ba�Bb(Bb�Bd�BgaBi'Be�Bd�Be'BfBgJBg�BhBg�Bf�Bg�BgBfNBe�Be�BeZBe Be6BfKBf�Bh�Bj$Bj�Bk�Bk+Bf�Bg�Bh�BmBq�Bu'BucBu�Bu�BvrBw�Bw�BweBpBl�Bi(BgBfBg�Bj�Bg�BWBWBV�BX*BZ2B]B]�B^�B^�B_�BaKBa�B`�B_�B_�B]B\�BX!BW$BV�BV�BV{BW�BY�B\�B\OB]hBa�Bc%Bd�Bf'BiBm�Bp�Bu+BxB~7BaB��B��B��B�;B�)B��B�TB��B��B��B�B��B�8B�dB��B��B�B�bB�=B��B��B��B��B�B�GBB��B�{B�B�B�bB�yB��B�hB�~B��BզBֻB�kB��B�SB��B��B�eB�bB��B�zB	 B�B	B	mB	�B	�B	�B	�B	�B	?B	�B	�B	�B	 8B	)B	*B	+B	/�B	.?B	/XB	1pB	2�B	3�B	5B	7MB	5�B	5�B	7�B	9�B	:�B	<7B	?B	@�B	C�B	F�B	KkB	M@B	MJB	M-B	NB	O=B	PVB	R�B	UB	X�B	]xB	`%B	aB	b�B	e�B	g�B	i�B	i�B	j�B	k�B	mB	o�B	qxB	q�B	r�B	r�B	r%B	srB	s�B	u�B	xHB	{oB	~>B	~]B	��B	��B	�B	�RB	��B	��B	�lB	��B	��B	�_B	�B	�B	�B	�B	�9B	�IB	��B	��B	�BB	�BB	�FB	�pB	��B	�dB	��B	��B	� B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	�;B	�B	�B	��B	��B	�B	��B	��B	�4B	�NB	�BB	�MB	�9B	�aB	�FB	�HB	�AB	�PB	�PB	�fB	�UB	�>B	�iB	��B	�;B	�B	�+B	�B	��B	��B	�	B	�B	�B	��B	��B	�aB	�sB	�B	�B	��B	�B	�	B	��B	�B	�B	�B	�B	�B	�3B	�"B	�(B	�B	�3B	�&B	�B	�`B	��B	��B	��B	�BB	�RB	�3B	�@B	��B	��B	�B
 �B
aB
uB
�B
kB
jB
zB
�B
�B
		B
	�B

�B

�B
�B
�B
�B
fB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
#B
AB
�B
B
�B
B
QB
�B
�B
�B
!B
�B
�B
B
B
B
(B
B
2B
<B
_B
NB
8B
QB
8B
B
!B
B
MB
IB
B
!B
7B
 B
 B
 B
 1B
 6B
 OB
!4B
!,B
!B
!B
!-B
"*B
"!B
" B
"B
#%B
##B
#.B
#%B
#B
#(B
#1B
#&B
#EB
$#B
$'B
$#B
$CB
$8B
$7B
%4B
%(B
%CB
%KB
%2B
%,B
&\B
&qB
&]B
&;B
&;B
&QB
&OB
&7B
'+B
'5B
'HB
'8B
'AB
'_B
',B
'LB
'kB
(sB
(�B
)�B
)BB
)AB
)CB
)yB
*�B
*\B
*]B
*ZB
*jB
*�B
+JB
+KB
+`B
+YB
+wB
+xB
,�B
,�B
-�B
.}B
.tB
.�B
/�B
/qB
/sB
/eB
/�B
0�B
1�B
1�B
1�B
2�B
2�B
2�B
2�B
2�B
3�B
4B
4�B
5�B
5�B
6�B
6�B
6�B
6�B
6�B
7�B
7�B
7�B
8B
8�B
8�B
8�B
9�B
9�B
9�B
:0B
:�B
:�B
:�B
:�B
:�B
:�B
;�B
<B
<�B
<�B
<�B
<�B
<�B
<�B
=�B
=�B
=�B
>.B
>�B
>�B
>�B
>�B
>�B
>�B
?B
?�B
?�B
?�B
@(B
?�B
?�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
A4B
BB
B�B
B�B
C'B
CB
DB
D"B
DxB
E<B
FB
FB
FB
GB
F�B
F�B
G B
F�B
F�B
F�B
GB
GB
HXB
H�B
I2B
IB
I B
IB
IB
I#B
J>B
JBB
JNB
KPB
KB
LB
LB
LB
L*B
L$B
LuB
L�B
MJB
N4B
N8B
N#B
NSB
NiB
O=B
O5B
O@B
OIB
OB
O$B
O,B
O|B
P�B
PWB
P�B
QpB
QCB
QbB
QJB
REB
R:B
RIB
RPB
R:B
R+B
R8B
REB
RTB
RaB
R�B
S�B
S�B
T^B
TGB
TGB
TQB
TTB
TwB
U}B
UlB
U�B
VyB
V�B
V�B
VgB
VTB
V]B
WoB
WcB
WhB
W�B
W�B
XiB
XRB
X]B
X\B
YYB
YhB
XyB
Y�B
Y�B
Z�B
Z�B
[�B
[zB
[oB
[sB
[�B
[�B
[�B
\�B
\zB
\�B
\�B
]�B
]�B
^�B
^vB
^wB
^�B
^sB
^vB
^wB
^�B
^sB
^xB
^�B
^�B
_�B
_�B
_zB
_zB
_{B
_�B
_�B
_�B
_�B
`�B
_�B
`�B
`�B
_�B
_�B
`�B
`�B
`�B
`�B
`�B
`�B
`yB
`�B
`�B
`�B
`�B
`�B
a�B
a�B
`�B
`�B
a�B
`�B
`�B
`�B
`�B
`�B
a B
a�B
a�B
a�B
a�B
a�B
a�B
a�B
a�B
a�B
b�B
b�B
b�B
b�B
b�B
b�B
b�B
c�B
c�B
c�B
dB
e*B
e�B
fB
e�B
e�B
e�B
e�B
e�B
e�B
f�B
f�B
f�B
f�B
f�B
g�B
g�B
g�B
g�B
g�B
g�B
g�B
g�B
g�B
h�B
h�B
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
i�B
j�B
j�B
j�B
j�B
j�B
j�B
lB
k�B
k�B
k�B
k�B
l B
k�B
l�B
l�B
mB
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
n�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<Pn<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<(��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<0��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<IZ�<'�w<#�
<#�
<#�
<#�
<#�
<#�
</��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - (REPORTED_SURFACE_PRESSURE) for PADJ                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al,2007,JAOT & effects of pressure adjustments                                                                                                                                                            PADJ REPORTED_SURFACE_PRESSURE =0.49 dbar                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CTM alpha = 0.141 & tau = 6.68 s with error equal to the correction                                                                                                                                                                                             Pressures adjusted using reported surface pressure when warranted.                                                                                                                                                                                              none                                                                                                                                                                                                                                                            Salinity corrected for pressure sensor calibration drift and cell thermal mass                                                                                                                                                                                  201603101135032016031011350320160310113503  AO  ARCAADJP                                                                    20140721233919    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721233919  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721233919  QCF$                G�O�G�O�G�O�0                                                                                                                                   G�O�G�O�G�O�                PM  ARSQPADJV1.1                                                                20160310113503  QC  PRES            @���D��G�O�                PM  ARSQCTM V1.1                                                                20160310113503  QC  PSAL            @���D��G�O�                PM  ARSQOWGUV1.0WOD + Argo                                                      20170523133337  IP                  G�O�G�O�G�O�                