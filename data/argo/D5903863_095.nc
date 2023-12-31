CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       d2014-08-18T18:07:26Z creation; 2014-08-18T18:07:26Z updated; 2015-09-28T12:13:24Z converted from 2.2   
references        (http://www.argodatamgt.org/Documentation   comment           user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7    PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7@   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8    PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8$   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8D   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8d   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   axis      T      units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�E�vQ�   
_FillValue        A.�~            8h   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8p   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�E�vQ�   
_FillValue        A.�~            8t   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   axis      Y      units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�             8|   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      axis      X      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�             8�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    standard_name         sea_water_pressure     C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  IL   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    standard_name         sea_water_pressure     C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M8   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  `�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      standard_name         sea_water_temperature      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  pl   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      standard_name         sea_water_temperature      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL         
      	   	long_name         Practical salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     standard_name         sea_water_salinity     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �0   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     standard_name         sea_water_salinity     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �d   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �P   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �$   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �$   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �$   SCIENTIFIC_CALIB_DATE               	             
_FillValue               	long_name         Date of calibration    conventions       YYYYMMDDHHMISS        ,  �$   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �P   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �T   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �X   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �\   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �`   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5903863 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               _A   AO  20140818180726  20170523133348  4298_0127_095                   2C  D   NAVIS_A                         0127                            120111                          863 @�^���
1   @�_c]��@7�KƧ��c|�/��1   GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                      A   A   A   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(fD(� D)  D)� D*  D*�fD+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DNy�DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̃3D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�I�D�l�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @z=q@�Q�@�Q�A(�A8(�AX(�Ax(�A�{A�{A�{A�{A�{A�{A�{A�{B
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
=B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HD `�D �D`�D�D`�D�D`�D�D`�D�D`�D�D`�D�D`�D�D`�D�D	`�D	�D
`�D
�D`�D�
D`�D�D`�D�D`�D�D`�D�D`�D�D`�D�D`�D�D`�D�D`�D�D`�D�D`�D�D`�D�D`�D�D`�D�D`�D�D`�D�D`�D�D`�D�D`�D�D`�D�D `�D �D!`�D!�D"`�D"�D#`�D#�D$`�D$�D%`�D%�D&`�D&�D'`�D'�
D(`�D(�D)`�D)�D*g
D*�D+`�D+�D,`�D,�D-`�D-�D.`�D.�D/`�D/�D0`�D0�D1`�D1�D2`�D2�D3`�D3�D4`�D4�D5`�D5�D6`�D6�D7`�D7�D8`�D8�D9`�D9�D:`�D:�D;`�D;�D<`�D<�D=`�D=�D>`�D>�D?`�D?�D@`�D@�DA`�DA�DB`�DB�DC`�DC�DD`�DD�DE`�DE�DF`�DF�DG`�DG�DH`�DH�DI`�DI�DJ`�DJ�DK`�DK�DL`�DL�DM`�DM�DNZ>DN�DO`�DO�DP`�DP�DQ`�DQ�DR`�DR�DS`�DS�DT`�DT�DU`�DU�DV`�DV�DW`�DW�DX`�DX�DY`�DY�DZ`�DZ�D[`�D[�D\`�D\�D]`�D]�D^`�D^�D_`�D_�D``�D`�Da`�Da�Db`�Db�Dc`�Dc�Dd`�Dd�De`�De�Df`�Df�Dg`�Dg�Dh`�Dh�Di`�Di�Dj`�Dj�Dk`�Dk�Dl`�Dl�Dm`�Dm�Dn`�Dn�Do`�Do�Dp`�Dp�Dq`�Dq�Dr`�Dr�Ds`�Ds�Dt`�Dt�Du`�Du�Dv`�Dv�Dw`�Dw�Dx`�Dx�Dy`�Dy�Dz`�Dz�D{`�D{�D|`�D|�D}`�D}�D~`�D~�D`�D�D�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD°RD��RD�0RD�pRDðRD��RD�0RD�pRDİRD��RD�0RD�pRDŰRD��RD�0RD�pRDưRD��RD�0RD�pRDǰRD��RD�0RD�pRDȰRD��RD�0RD�pRDɰRD��RD�0RD�pRDʰRD��RD�0RD�pRD˰RD��RD�0RD�s�D̰RD��RD�0RD�pRDͰRD��RD�0RD�pRDΰRD��RD�0RD�pRDϰRD��RD�0RD�pRDаRD��RD�0RD�pRDѰRD��RD�0RD�pRDҰRD��RD�0RD�pRDӰRD��RD�0RD�pRD԰RD��RD�0RD�pRDհRD��RD�0RD�pRDְRD��RD�0RD�pRDװRD��RD�0RD�pRDذRD��RD�0RD�pRDٰRD��RD�0RD�pRDڰRD��RD�0RD�pRD۰RD��RD�0RD�pRDܰRD��RD�0RD�pRDݰRD��RD�0RD�pRDްRD��RD�0RD�pRD߰RD��RD�0RD�pRD�RD��RD�0RD�pRD�RD��RD�0RD�pRD�RD��RD�0RD�pRD�RD��RD�0RD�pRD�RD��RD�0RD�pRD�RD��RD�0RD�pRD�RD��RD�0RD�pRD�RD��RD�0RD�pRD�RD��RD�0RD�pRD�RD��RD�0RD�pRD�RD��RD�0RD�pRD�RD��RD�0RD�pRD�RD��RD�0RD�pRD��RD��RD�0RD�pRD�RD��RD�0RD�pRD�RD��RD�0RD�pRD�RD��RD�0RD�pRD�RD��RD�0RD�pRD�RD��RD�0RD�pRD�RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�9�D�]11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A��A�"�A�&�A�(�A�&�A�&�A�&�A�&�A�+A�(�A�(�A�(�A�(�A�+A�+A�+A�1'A�(�A�(�AͶFA�ȴAɇ+A�M�A�bAǸRA��AőhA�bNA���A��jA�p�A�oA���A�1'A��RA��-A�K�A���A�z�A�E�A�VA�{A��DA��9A���A��+A�v�A�&�A��A�"�A���A��A�M�A�A���A��uA�VA��hA�?}A���A�\)A��A��-A�O�A��wA��RA�l�A�XA�K�A�=qA��A��`A�I�A���A���A��hA��A�t�A�=qA�VA���A�oA�z�A�hsA�1A���A��FA�G�A�Q�A��A�oA�7LA��A�%A�1'A�jA� �A���A���A�/A�ƨA�9XA���A���A��^A�C�A�VA}p�Az�Avz�As;dAq��Ao�mAk&�Ai�PAh �Ac33Aa�A_/A^~�A]��A\M�A[�7A[�wA\VA[��AZZAXVAW�wAV^5AW/AV�DAS�7AR��AP�AO
=AM�PAK�AJ�/AJ1'AH-AFM�AEVAC
=AA��AAS�A@��A?�A>��A=��A=A9\)A7+A6��A6�!A6��A5�wA4�A3��A1ƨA0�!A0I�A0-A/��A/VA.5?A-l�A-
=A+��A*�uA*1A)\)A(�/A(Q�A'��A&�HA%�TA$��A#��A"5?A!�mA ��A�;A��AA�A�A��A�A{A�\A�A��A;dAoAbNAdZAjA  A��A�HA~�AJA�A��AbA��AO�AjA��A
bA	&�A�/A=qAXA��AA��A��Ap�A~�A�Ao@��F@�O�@���@���@�Ĝ@��m@�33@�
=@��H@�v�@�x�@�O�@�%@��@�F@�J@�u@땁@�@�bN@�@�l�@�b@�l�@��y@���@�/@�ƨ@�v�@���@ܴ9@�1'@�+@�%@�1'@�"�@��H@��#@�Q�@�I�@���@ЋD@�C�@�C�@θR@͙�@̛�@�&�@��@��y@�^5@Ł@�bN@�t�@��#@�ƨ@�ȴ@�@���@�l�@��\@�j@�{@��7@��@�r�@���@�{@���@�Z@�A�@��@��@��-@���@�$�@�@�S�@�ȴ@�@��@��/@��u@�z�@�bN@�Z@�Q�@�ȴ@�z�@���@�"�@�@���@� �@���@��y@���@�l�@�dZ@��@�1'@�Z@���@�%@��h@��+@���@�z�@�1'@�1'@� �@�j@�A�@� �@���@�+@�o@��y@�@���@�5?@�-@�{@��D@��m@�33@��@�E�@���@���@�7L@��u@�I�@�9X@�A�@��w@�S�@�C�@�C�@�@���@��!@���@�$�@�{@��@���@�`B@�`B@�7L@��9@�bN@�1@�b@�1@��@�\)@�{@���@�J@���@��+@�ff@�$�@��@�@���@��h@�hs@�?}@��D@��@���@���@���@�\)@�"�@��H@���@��\@�v�@�E�@�5?@�-@��#@��7@��@�%@��`@���@��9@��@�j@�9X@�1'@��@�S�@��@�=q@�&�@�Ĝ@�Q�@���@�K�@�\)@���@��@�t�@�"�@��@��R@�hs@���@�  @�r�@�hs@�?}@��D@�z�@�j@�Z@��@�~�@�$�@�@�&�@��@��/@���@��D@�Z@���@���@��\@��!@��@�ƨ@��y@��@�G�@�7L@��@�/@���@�r�@��@�I�@���@��F@�\)@�@�ȴ@��\@���@�=q@��#@��^@��7@�p�@�`B@�`B@�`B@�X@�O�@�X@�O�@��@��@�A�@�(�@� �@|�@~ȴ@~��@~��@~��@~��@~ff@|��@|I�@|1@{t�@{33@{C�@{�@{��@|z�@|�j@|��@|��@|9X@{�m@{t�@{"�@z��@z=q@z�@y��@y�@y��@y�^@yhs@x�@wl�@vE�@t�/@s��@r��@qhs@p �@o�@o\)@n�@n��@nv�@nE�@n5?@n$�@nE�@n5?@m`B@l��@k�m@k��@k��@kt�@k�@k�@kS�@k��@kƨ@k�m@l(�@lI�@lj@l��@l�D@lZ@l�@k�m@k��@j�@j��@j��@j�\@j~�@j~�@jn�@j�@i�#@i�7@ihs@i7L@h�`@hr�@h1'@g|�@g
=@f�@f��@fȴ@fff@e@d��@dZ@d1@dZ@d1@ct�@b�@b-@a��@a��@ax�@a&�@`�@`r�@`r�@`bN@`�@`r�@`bN@_�@_+@_K�@_�@_�;@_��@_�@_�P@_K�@_
=@^{@\�@\��@[�m@[�F@[��@[33@["�@["�@Z�H@Z^5@Y��@Y7L@Y&�@YG�@Y%@Y�@Y%@X��@XQ�@W��@Wl�@WK�@V�@Vv�@Vv�@Vv�@Vv�@VV@U�h@U?}@T��@T�@Tz�@TI�@T9X@S��@R��@Rn�@RJ@Q��@Qhs@PA�@O�P@O;d@O;d@O;d@N��@M�T@M�h@M�@MV@L��@L�@L�j@L�D@Lj@K�m@K�@KdZ@K@J��@J��@J=q@J-@JJ@JJ@I��@IG�@H �@G�@G�P@G|�@G|�@G\)@G;d@G�@G
=@F�R@F{@E�@E��@Ep�@EV@D��@DZ@D1@C��@C�m@C�m@C�
@C�@CS�@CC�@C33@C"�@C@B�@B�H@B�H@B��@B�!@B��@B^5@B-@A�@@ �@?�;@?�@?+@>��@>�+@>v�@>E�@>$�@=�@=�@=V@<�/@<�@<z�@<9X@;�F@;�@;"�@:�H@9�#@9hs@8��@8�9@8��@8 �@7�P@7
=@6��@6ff@6V@5�@5�-@5��@5�@5O�@4�/@4I�@3�
@3t�@3t�@3dZ@3dZ@333@3o@2�H@2�\@2n�@2J@1�^@0��@0�u@01'@0  @/�@/��@/�;@/�;@/�;@/
=@.ff@.$�@-�T@-�@-/@,�/@,z�@,Z@+��@+t�@+S�@+"�@*�@*�!@*�\@*~�@*M�@*J@)��@)�@)��@)�7@)7L@)%@)%@(��@(��@(��@(�@(�@(�@(A�@'��@'l�@'+@&��@'�@';d@'+@&�y@&�+@&{@%��@%��@%�@%`B@%?}@$�@$�j@$�@$j@$(�@$1@#�@#"�@#@"�H@"�H@"�!@"^5@"=q@"J@!��@!��@!�7@!hs@!�@!%@ ��@ �`@ Ĝ@ A�@  �@�w@l�@+@ȴ@v�@ff@ff@ff@V@E�@E�@$�@{@@/@��@�j@��@��@�D@�D@j@�m@��@t�@dZ@C�@"�@@@�@�!@�!@��@~�@M�@=q@-@��@�#@�^@x�@x�@hs@hs@7L@�`@�@bN@bN@A�@b@�@�w@��@K�@�y@�R@�+@�+@ff@E�@5?@$�@�@@�-@�@p�@O�@�@�/@�D@I�@�@��@�F@�H@�\@^5@=q@-@�@�@�^@x�@7L@�`@�u@r�@bN@A�@�;@�w@�@��@��@��@��@�P@|�@;d@+@�@ȴ@ȴ@�R@ff@E�@$�@�T@@��@�h@�@`B@�@�@�@��@�D@Z@9X@(�@�@�@��@ƨ@ƨ@�F@��@�@�@t�@S�@C�@"�@o@@
��@
��@
�!@
�\@
�\@
n�@
n�@
n�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��A��A�"�A�&�A�(�A�&�A�&�A�&�A�&�A�+A�(�A�(�A�(�A�(�A�+A�+A�+A�1'A�(�A�(�AͶFA�ȴAɇ+A�M�A�bAǸRA��AőhA�bNA���A��jA�p�A�oA���A�1'A��RA��-A�K�A���A�z�A�E�A�VA�{A��DA��9A���A��+A�v�A�&�A��A�"�A���A��A�M�A�A���A��uA�VA��hA�?}A���A�\)A��A��-A�O�A��wA��RA�l�A�XA�K�A�=qA��A��`A�I�A���A���A��hA��A�t�A�=qA�VA���A�oA�z�A�hsA�1A���A��FA�G�A�Q�A��A�oA�7LA��A�%A�1'A�jA� �A���A���A�/A�ƨA�9XA���A���A��^A�C�A�VA}p�Az�Avz�As;dAq��Ao�mAk&�Ai�PAh �Ac33Aa�A_/A^~�A]��A\M�A[�7A[�wA\VA[��AZZAXVAW�wAV^5AW/AV�DAS�7AR��AP�AO
=AM�PAK�AJ�/AJ1'AH-AFM�AEVAC
=AA��AAS�A@��A?�A>��A=��A=A9\)A7+A6��A6�!A6��A5�wA4�A3��A1ƨA0�!A0I�A0-A/��A/VA.5?A-l�A-
=A+��A*�uA*1A)\)A(�/A(Q�A'��A&�HA%�TA$��A#��A"5?A!�mA ��A�;A��AA�A�A��A�A{A�\A�A��A;dAoAbNAdZAjA  A��A�HA~�AJA�A��AbA��AO�AjA��A
bA	&�A�/A=qAXA��AA��A��Ap�A~�A�Ao@��F@�O�@���@���@�Ĝ@��m@�33@�
=@��H@�v�@�x�@�O�@�%@��@�F@�J@�u@땁@�@�bN@�@�l�@�b@�l�@��y@���@�/@�ƨ@�v�@���@ܴ9@�1'@�+@�%@�1'@�"�@��H@��#@�Q�@�I�@���@ЋD@�C�@�C�@θR@͙�@̛�@�&�@��@��y@�^5@Ł@�bN@�t�@��#@�ƨ@�ȴ@�@���@�l�@��\@�j@�{@��7@��@�r�@���@�{@���@�Z@�A�@��@��@��-@���@�$�@�@�S�@�ȴ@�@��@��/@��u@�z�@�bN@�Z@�Q�@�ȴ@�z�@���@�"�@�@���@� �@���@��y@���@�l�@�dZ@��@�1'@�Z@���@�%@��h@��+@���@�z�@�1'@�1'@� �@�j@�A�@� �@���@�+@�o@��y@�@���@�5?@�-@�{@��D@��m@�33@��@�E�@���@���@�7L@��u@�I�@�9X@�A�@��w@�S�@�C�@�C�@�@���@��!@���@�$�@�{@��@���@�`B@�`B@�7L@��9@�bN@�1@�b@�1@��@�\)@�{@���@�J@���@��+@�ff@�$�@��@�@���@��h@�hs@�?}@��D@��@���@���@���@�\)@�"�@��H@���@��\@�v�@�E�@�5?@�-@��#@��7@��@�%@��`@���@��9@��@�j@�9X@�1'@��@�S�@��@�=q@�&�@�Ĝ@�Q�@���@�K�@�\)@���@��@�t�@�"�@��@��R@�hs@���@�  @�r�@�hs@�?}@��D@�z�@�j@�Z@��@�~�@�$�@�@�&�@��@��/@���@��D@�Z@���@���@��\@��!@��@�ƨ@��y@��@�G�@�7L@��@�/@���@�r�@��@�I�@���@��F@�\)@�@�ȴ@��\@���@�=q@��#@��^@��7@�p�@�`B@�`B@�`B@�X@�O�@�X@�O�@��@��@�A�@�(�@� �@|�@~ȴ@~��@~��@~��@~��@~ff@|��@|I�@|1@{t�@{33@{C�@{�@{��@|z�@|�j@|��@|��@|9X@{�m@{t�@{"�@z��@z=q@z�@y��@y�@y��@y�^@yhs@x�@wl�@vE�@t�/@s��@r��@qhs@p �@o�@o\)@n�@n��@nv�@nE�@n5?@n$�@nE�@n5?@m`B@l��@k�m@k��@k��@kt�@k�@k�@kS�@k��@kƨ@k�m@l(�@lI�@lj@l��@l�D@lZ@l�@k�m@k��@j�@j��@j��@j�\@j~�@j~�@jn�@j�@i�#@i�7@ihs@i7L@h�`@hr�@h1'@g|�@g
=@f�@f��@fȴ@fff@e@d��@dZ@d1@dZ@d1@ct�@b�@b-@a��@a��@ax�@a&�@`�@`r�@`r�@`bN@`�@`r�@`bN@_�@_+@_K�@_�@_�;@_��@_�@_�P@_K�@_
=@^{@\�@\��@[�m@[�F@[��@[33@["�@["�@Z�H@Z^5@Y��@Y7L@Y&�@YG�@Y%@Y�@Y%@X��@XQ�@W��@Wl�@WK�@V�@Vv�@Vv�@Vv�@Vv�@VV@U�h@U?}@T��@T�@Tz�@TI�@T9X@S��@R��@Rn�@RJ@Q��@Qhs@PA�@O�P@O;d@O;d@O;d@N��@M�T@M�h@M�@MV@L��@L�@L�j@L�D@Lj@K�m@K�@KdZ@K@J��@J��@J=q@J-@JJ@JJ@I��@IG�@H �@G�@G�P@G|�@G|�@G\)@G;d@G�@G
=@F�R@F{@E�@E��@Ep�@EV@D��@DZ@D1@C��@C�m@C�m@C�
@C�@CS�@CC�@C33@C"�@C@B�@B�H@B�H@B��@B�!@B��@B^5@B-@A�@@ �@?�;@?�@?+@>��@>�+@>v�@>E�@>$�@=�@=�@=V@<�/@<�@<z�@<9X@;�F@;�@;"�@:�H@9�#@9hs@8��@8�9@8��@8 �@7�P@7
=@6��@6ff@6V@5�@5�-@5��@5�@5O�@4�/@4I�@3�
@3t�@3t�@3dZ@3dZ@333@3o@2�H@2�\@2n�@2J@1�^@0��@0�u@01'@0  @/�@/��@/�;@/�;@/�;@/
=@.ff@.$�@-�T@-�@-/@,�/@,z�@,Z@+��@+t�@+S�@+"�@*�@*�!@*�\@*~�@*M�@*J@)��@)�@)��@)�7@)7L@)%@)%@(��@(��@(��@(�@(�@(�@(A�@'��@'l�@'+@&��@'�@';d@'+@&�y@&�+@&{@%��@%��@%�@%`B@%?}@$�@$�j@$�@$j@$(�@$1@#�@#"�@#@"�H@"�H@"�!@"^5@"=q@"J@!��@!��@!�7@!hs@!�@!%@ ��@ �`@ Ĝ@ A�@  �@�w@l�@+@ȴ@v�@ff@ff@ff@V@E�@E�@$�@{@@/@��@�j@��@��@�D@�D@j@�m@��@t�@dZ@C�@"�@@@�@�!@�!@��@~�@M�@=q@-@��@�#@�^@x�@x�@hs@hs@7L@�`@�@bN@bN@A�@b@�@�w@��@K�@�y@�R@�+@�+@ff@E�@5?@$�@�@@�-@�@p�@O�@�@�/@�D@I�@�@��@�F@�H@�\@^5@=q@-@�@�@�^@x�@7L@�`@�u@r�@bN@A�@�;@�w@�@��@��@��@��@�P@|�@;d@+@�@ȴ@ȴ@�R@ff@E�@$�@�T@@��@�h@�@`B@�@�@�@��@�D@Z@9X@(�@�@�@��@ƨ@ƨ@�F@��@�@�@t�@S�@C�@"�@o@@
��@
��@
�!@
�\@
�\@
n�@
n�@
n�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBB�BB�BB�BA�BA�BA�BB�BB�BB�BA�BB�BB�BB�BB�BB�BB�BA�B@�B>wBM�B�fBR�BF�B9XB33B,B�BB�;BhBYBI�B$�B�B�B$�B2-BM�B]/BM�B<jB-B�BoB �B=qBJ�BJ�BH�B7LB �B1B��B�`B�#B��B�qB�LB�9B�XB�B��B��B��B��B�uB�+B�=B�DB�DB�7B�%B�B|�Bp�BffB^5BVBL�BI�BF�B@�B7LB+B�BJB%BB��B��B�^B��B��B�=B~�Bn�BaHBH�B1'B�BPB
�B
�dB
��B
x�B
cTB
G�B
/B
	7B	�B	��B	�?B	��B	��B	v�B	gmB	\)B	:^B	/B	�B	�B	�B	\B	�B	�B	8RB	8RB	%�B	hB	�B	VB	%�B	.B	#�B	�B	�B	VB	+B��B��B��B�B�`B�;B�#B��B��B��B��B��BǮBĜB�jB�FB�?B�9B�-B�!B�B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�uB�hB�\B�PB�=B�+B�B� B}�B{�Bw�Bu�Br�Bl�BhsBe`BaHB_;B_;B^5B]/B\)BZBW
BVBT�BS�BR�BQ�BP�BO�BO�BO�BM�BK�BJ�BI�BG�BF�BD�BC�BC�BC�BA�B=qB9XB7LB5?B2-B.B/B0!B/B/B.B.B/B/B/B0!B0!B/B-B1'B0!B0!B0!B0!B/B,B&�B(�B1'B2-B2-B2-B2-B2-B33B33B49B49B49B/B+B+B,B,B,B/B)�B'�B-B5?B8RB6FB5?B+B,B.B33B;dB8RB:^BB�BB�BC�BF�BF�BE�BC�BB�BF�BG�BH�BI�BK�BO�BVBXB\)BffBm�Bz�B}�B�B�=B�VB�oB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�!B�XBÖB��B��B��B��B�B�5B�BB�`B�sB�B�B�B�B��B��B��B��B��B��B��B	  B	B	B	+B	PB	uB	{B	�B	�B	!�B	#�B	'�B	.B	49B	7LB	:^B	@�B	A�B	B�B	C�B	C�B	D�B	F�B	G�B	H�B	I�B	O�B	Q�B	Q�B	Q�B	Q�B	T�B	VB	[#B	]/B	^5B	`BB	aHB	bNB	bNB	cTB	dZB	dZB	ffB	hsB	jB	m�B	m�B	p�B	q�B	q�B	r�B	u�B	y�B	y�B	y�B	z�B	}�B	� B	�B	�B	�B	�+B	�1B	�=B	�DB	�DB	�=B	�DB	�PB	�JB	�JB	�=B	�=B	�1B	�=B	�JB	�PB	�bB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�-B	�-B	�'B	�-B	�9B	�9B	�?B	�?B	�?B	�?B	�?B	�?B	�?B	�?B	�FB	�FB	�^B	��B	ÖB	ÖB	B	ŢB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�B	�#B	�#B	�#B	�#B	�)B	�/B	�;B	�BB	�NB	�TB	�TB	�TB	�ZB	�`B	�fB	�mB	�mB	�fB	�fB	�fB	�fB	�mB	�mB	�mB	�sB	�sB	�mB	�mB	�mB	�sB	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
  B
B
B
B
B
B
B
%B
1B
	7B
DB
JB
VB
\B
\B
bB
bB
hB
hB
hB
uB
{B
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
"�B
$�B
$�B
#�B
#�B
$�B
&�B
'�B
'�B
'�B
'�B
'�B
&�B
%�B
$�B
#�B
"�B
"�B
$�B
%�B
%�B
%�B
&�B
&�B
&�B
'�B
(�B
)�B
)�B
,B
,B
-B
-B
-B
-B
.B
.B
/B
/B
/B
/B
/B
1'B
1'B
1'B
1'B
2-B
2-B
2-B
1'B
1'B
1'B
1'B
2-B
33B
33B
2-B
2-B
2-B
2-B
2-B
33B
33B
33B
49B
5?B
5?B
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
7LB
7LB
8RB
9XB
9XB
9XB
9XB
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
:^B
:^B
:^B
;dB
;dB
;dB
;dB
;dB
;dB
;dB
<jB
<jB
<jB
<jB
<jB
=qB
=qB
=qB
=qB
=qB
=qB
=qB
=qB
>wB
>wB
>wB
>wB
?}B
?}B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
A�B
A�B
B�B
B�B
C�B
C�B
C�B
D�B
C�B
D�B
E�B
E�B
E�B
F�B
E�B
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
H�B
I�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
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
P�B
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
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
T�B
T�B
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
W
B
W
B
W
B
W
B
XB
YB
YB
YB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
[#B
[#B
[#B
[#B
\)B
\)B
\)B
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
aHB
bNB
bNB
bNB
bNB
bNB
cTB
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
ffB
ffB
ffB
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
jB
jB
jB
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
n�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
o�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   BB�BB�BB�BA�BA�BA�BB�BB�BB�BA�BB�BB�BB�BB�BB�BB�BA�B@�BB<B^#B�B[vBKVB:TB4~B.nB �B�B��BKBaQBN�B)UB"CB(�B(�B3�BSBb3BR3B@�B0�B$�BkB!B>BK(BLBN�B<UB%`B�B��B�B�vB��B��B�~B��B��B��B��B�B��B�}B�,B��B��B��B��B�B�PB�B�_Bs�Bh�B`�BY)BNBJ�BH�BCUB:HB03BwBpB�B]B �B�B��B�B�B��B�2Br~BgyBN�B5�B!�B�B
�%B
æB
�	B
~�B
jlB
M�B
:NB
�B	�XB	�B	��B	��B	��B	z�B	k]B	g�B	={B	5�B	!~B	�B	[B	B	DB	�B	9LB	<B	*]B	�B	�B	�B	'�B	4�B	&B	!�B	�B	�B	�B	B��B��B��B�B�dB�=B�NB�TB�5B��B�BɚB�yB��B�dB��B��B��B��B��B��B��B� B�gB�	B��B�B��B��B�/B�IB�B�`B��B��B�B��B��B��B��B��B��B��B~�BzBw�BwBn�BjHBi6Bd�B`�B`�B_2B]�B^B\�BY�BWIBVFBVBT"BSTBR�BQ�BRJBP�BOcBNSBL:BN�BJ&BG�BFqBFBE�BEfBD�B@	B:�B9�B6�B4kB1CB2>B2!B1�B0cB/UB/ B/�B/�B/�B1�B0�B/�B2B2�B2gB29B1�B2bB1nB0�B(�B(hB2IB3"B3�B3�B4hB43B5}B3�B58B5�B7mB0�B,�B+�B-�B.\B,vB2�B,AB)�B-SB6FB:&B7�B:B,�B-�B/B4�B=B9�B<�BE^BDBD�BG�BHbBF�BF BE<BGxBHeBI�BJ�BM�BQ�BV�BXkB\BfBl�Bz�B}�B� B�B�gB�B��B�)B�HB�B�B��B�B�QB�TB��B�GB��B�mB�%B��B��B�WB��B�MB��B��B��B��B��B�PB��B��BBψB�.B�LB��BڛBޭB�KB�<B��B�B�B�BB�B�B�CB�CB�	B�B��B��B	 �B	�B	�B	CB	�B	�B	�B	uB	wB	"!B	$B	(�B	.�B	4pB	7�B	;B	@�B	A�B	C]B	DB	C�B	EB	GxB	HJB	I\B	I�B	P)B	R�B	R�B	S�B	RJB	U"B	U|B	[�B	]�B	^�B	`�B	a�B	b�B	b�B	c�B	d�B	e�B	g}B	iB	j�B	m�B	n?B	q>B	rSB	rNB	sB	v+B	zfB	z5B	z0B	{�B	~�B	�VB	��B	�B	��B	�SB	��B	��B	��B	��B	��B	�uB	�OB	��B	�-B	�"B	�8B	��B	��B	�pB	�;B	��B	�B	�=B	�JB	�EB	�B	�B	��B	�]B	��B	��B	�[B	�{B	��B	��B	��B	��B	�B	�	B	�WB	��B	��B	��B	��B	��B	�B	� B	��B	�OB	�xB	�}B	�B	��B	��B	��B	ȕB	�nB	ͩB	�;B	�B	�bB	ҁB	�qB	ՠB	֣B	׈B	׍B	�8B	��B	��B	ۍB	۞B	܃B	݄B	�|B	��B	�B	�B	�B	�B	�#B	�0B	��B	��B	�B	�-B	�B	�B	�B	�B	�B	��B	��B	�B	��B	�B	��B	�B	�B	��B	�DB	��B	�B	��B	�vB	�RB	�gB	�RB	�^B	��B	�IB	�RB	�GB
 YB
XB
�B
�B
B
$B
 DB	�B	��B	�B	�B	�oB	�[B	��B	�MB	�MB	�LB	�;B	�?B
 ,B
 TB
 �B
 �B
 �B
uB
UB
yB
KB
dB
�B
5B
MB
	`B
UB
rB
�B
}B
�B
�B
�B
�B
�B
B
�B
�B
�B
�B
�B
�B
B
�B
B
�B
�B
B
&B
B
ZB
3B
�B
�B
B
?B
rB
�B
wB
+B
�B
 EB
!xB
!rB
!�B
!,B
!AB
!-B
"QB
"�B
"B
"B
"B
!�B
#!B
%2B
%�B
${B
#�B
$�B
'
B
(6B
(DB
(EB
(WB
(XB
'�B
&�B
%SB
$�B
#0B
# B
%gB
&1B
&'B
&PB
'�B
'�B
'hB
(<B
)"B
*dB
*3B
,TB
,kB
-�B
-�B
-�B
-gB
.�B
.�B
/^B
/\B
/^B
/uB
/�B
1�B
1�B
1uB
1�B
2�B
2}B
2�B
2B
1�B
1�B
1�B
2�B
4:B
3�B
2�B
2mB
2uB
2�B
2�B
3�B
3�B
3�B
4�B
5�B
5�B
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
7�B
7�B
7�B
8�B
:pB
9�B
9�B
9�B
9�B
9�B
9�B
9�B
:�B
:�B
;B
:�B
:�B
:�B
:�B
:�B
:�B
;�B
;�B
;�B
;�B
;�B
;�B
;�B
<�B
<�B
<�B
<�B
<�B
=�B
=�B
=�B
=�B
=�B
=�B
=�B
>�B
?tB
>�B
>�B
?#B
@(B
?�B
@�B
@�B
@�B
A�B
B!B
BB
A�B
B�B
B�B
CB
C6B
A�B
BB
CB
C�B
D3B
D-B
D	B
D�B
DBB
EXB
FLB
F8B
FB
F�B
F5B
GB
F�B
GB
GB
GDB
HeB
HIB
I@B
H�B
I B
H�B
IB
IB
IB
J.B
JB
J8B
J+B
JwB
J6B
K?B
K B
KB
KB
J�B
K B
KB
K�B
KmB
K/B
K-B
KCB
K:B
K6B
KDB
KB
KB
L.B
L!B
L*B
L*B
L5B
L B
LB
L-B
L9B
MB
MB
M)B
M?B
MOB
N7B
NB
N B
N0B
N6B
N-B
NB
NB
NKB
NjB
NdB
ORB
O>B
PB
PB
Q3B
QVB
QlB
QkB
QNB
QCB
Q8B
Q<B
Q8B
QYB
RIB
R6B
RQB
RSB
RDB
RB
RjB
SJB
SGB
S4B
STB
ScB
SHB
STB
SZB
TNB
TLB
TPB
TmB
TBB
UEB
UGB
USB
U�B
UTB
U�B
VxB
VnB
V�B
VzB
WUB
WKB
WKB
WVB
WUB
WIB
WaB
WWB
W�B
X�B
YwB
Y�B
YnB
Z[B
ZdB
Z[B
ZxB
Z�B
Z�B
ZsB
ZiB
ZtB
ZxB
[yB
[cB
[nB
[�B
\iB
\vB
\�B
\�B
\wB
\sB
\�B
]�B
]�B
]�B
]rB
]yB
]mB
]�B
]�B
^�B
^�B
^vB
^�B
_�B
_�B
_�B
_�B
_�B
_�B
`�B
a�B
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
c�B
c�B
c�B
c�B
d8B
d�B
e�B
e�B
e�B
e�B
e�B
e�B
e�B
f�B
f�B
f�B
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
j�B
j�B
j�B
lB
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
n�B
n�B
n�B
n�B
oB
n�B
o�B
o�B
o�B
o�B
o�B
o�B
o�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<���<#�
<#�
<#�
<#�
<#�
<#�
<#�
<,�}<#�
<#�
<#�
<#�
<#�
<#�
<$&�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<'�w<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<J'�<#�
<#�
<#�
<#�
<#�
<E�<#�
<#�
<#�
<#�
<#�
<G��<#�
<#�
<Gy
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - (REPORTED_SURFACE_PRESSURE) for PADJ                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al,2007,JAOT & effects of pressure adjustments                                                                                                                                                            PADJ REPORTED_SURFACE_PRESSURE =0.49 dbar                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CTM alpha = 0.141 & tau = 6.68 s with error equal to the correction                                                                                                                                                                                             Pressures adjusted using reported surface pressure when warranted.                                                                                                                                                                                              none                                                                                                                                                                                                                                                            Salinity corrected for pressure sensor calibration drift and cell thermal mass                                                                                                                                                                                  201603101135252016031011352520160310113525  AO  ARGQ                                                                        20140818180726  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20140818180726  QCF$                G�O�G�O�G�O�0               PM  ARSQPADJV1.1                                                                20160310113525  QC  PRES            @���D�l�G�O�                PM  ARSQCTM V1.1                                                                20160310113525  QC  PSAL            @���D�l�G�O�                PM  ARSQOWGUV1.0WOD + Argo                                                      20170523133348  IP                  G�O�G�O�G�O�                