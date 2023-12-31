CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       d2014-07-21T23:39:44Z creation; 2014-07-21T23:39:44Z updated; 2015-09-28T12:13:13Z converted from 3.0   
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
_FillValue                 �  I<   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M$   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  `�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p4   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �D   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �,   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �T   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �<   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ͸   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �L   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �|   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �|   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �|   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �|   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �    HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �$   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �(Argo profile    3.1 1.2 19500101000000  20140721233944  20170523133347  5903863 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               \A   AO  4298_0127_092                   2C  D   NAVIS_A                         0127                            120111                          863 @����1   @���_ @7=�E���c�I�^5?1   GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                      \A   A   A   @�33@�  A   A   A@  A`  A~ffA�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D	  D	� D
fD
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DXfDX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�C3Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�3D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�|�D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@�Q�@�Q�A(�A8(�AX(�Av�\A�{A�{A�{A�{A�{A�{A�{A�{B
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
=B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HD `�D �D`�D�D`�D�D`�D�D`�D�D`�D�>D`�D�D`�D�D`�D�D	`�D	�
D
`�D
�D`�D�D`�D�D`�D�D`�D�D`�D�D`�D�D`�D�D`�D�D`�D�D`�D�D`�D�D`�D�D`�D�D`�D�D`�D�D`�D�D`�D�D`�D�D`�D�D`�D�D`�D�D `�D �D!`�D!�D"`�D"�D#`�D#�D$`�D$�D%`�D%�D&`�D&�D'`�D'�D(`�D(�D)`�D)�D*`�D*�D+`�D+�D,`�D,�D-`�D-�D.`�D.�D/`�D/�D0`�D0�D1`�D1�D2`�D2�D3`�D3�D4`�D4�D5`�D5�D6`�D6�D7`�D7�D8`�D8�D9`�D9�D:`�D:�D;`�D;�D<`�D<�D=`�D=�D>`�D>�D?`�D?�D@`�D@�DA`�DA�DB`�DB�DC`�DC�DD`�DD�DE`�DE�DF`�DF�DG`�DG�DH`�DH�DI`�DI�DJ`�DJ�DK`�DK�DL`�DL�DM`�DM�DN`�DN�DO`�DO�DP`�DP�DQ`�DQ�DR`�DR�DS`�DS�DT`�DT�DU`�DU�DV`�DV�DW`�DW�
DX`�DX�DY`�DY�DZ`�DZ�D[`�D[�D\`�D\�D]`�D]�D^`�D^�D_`�D_�D``�D`�Da`�Da�Db`�Db�Dc`�Dc�Dd`�Dd�De`�De�Df`�Df�Dg`�Dg�Dh`�Dh�Di`�Di�Dj`�Dj�Dk`�Dk�Dl`�Dl�Dm`�Dm�Dn`�Dn�Do`�Do�Dp`�Dp�Dq`�Dq�Dr`�Dr�Ds`�Ds�Dt`�Dt�Du`�Du�Dv`�Dv�Dw`�Dw�Dx`�Dx�Dy`�Dy�Dz`�Dz�D{`�D{�D|`�D|�D}`�D}�D~`�D~�D`�D�D�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD°RD��RD�0RD�pRDðRD��RD�0RD�pRDİRD��RD�0RD�pRDŰRD��RD�0RD�pRDưRD��RD�0RD�pRDǰRD��RD�0RD�pRDȰRD��RD�0RD�pRDɰRD��RD�0RD�pRDʰRD��RD�0RD�pRD˰RD��RD�0RD�pRD̰RD��RD�0RD�pRDͰRD��RD�0RD�pRDΰRD��RD�0RD�pRDϰRD��RD�0RD�pRDаRD��RD�0RD�pRDѰRD��RD�0RD�pRDҰRD��RD�0RD�pRDӰRD��RD�0RD�pRD԰RD��RD�0RD�pRDհRD��RD�0RD�pRDְRD��RD�0RD�pRDװRD��RD�0RD�pRDذRD��RD�0RD�pRDٰRD��RD�0RD�pRDڰRD��RD�0RD�pRD۰RD��RD�0RD�pRDܰRD��RD�0RD�pRDݰRD��RD�3�D�pRDްRD��RD�0RD�pRD߰RD��RD�0RD�pRD�RD��RD�0RD�pRD�RD��RD�0RD�pRD�RD��RD�0RD�pRD�RD��RD�0RD�pRD�RD��RD�0RD�pRD�RD��RD�0RD�pRD�RD��RD�0RD�pRD�RD��RD�0RD�pRD�RD��RD�0RD�pRD�RD��D�0RD�pRD�RD��RD�0RD�pRD�RD��RD�0RD�pRD�RD��RD�0RD�pRD��RD��RD�0RD�mD�RD��RD�0RD�pRD�RD��RD�0RD�pRD�RD��RD�0RD�pRD�RD��RD�0RD�pRD�RD��RD�0RD�pRD�RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�3�D�pR1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�C�A�;dA�A�A�K�A�K�A�G�A�K�A�VA�S�A�S�A�XA�S�A�G�A�1AЬA�I�A�+A�A�bNA��A�dZA�;dA�%A���A�7LA�ZA��DA�bNA�O�A�=qA�{A��/A�ĜA�I�A�1'A���A��hA�\)A�ȴA�^5A���A���A��\A�ffA�A�A�`BA�^5A�ƨA�t�A�+A��7A��A�\)A���A��jA�33A��mA��RA���A�M�A�%A�ZA�^5A��A��A��^A��7A�K�A�VA��#A�A�p�A���A�|�A�7LA��A��TA���A�hsA�G�A� �A���A��PA���A� �A�{A�ȴA��A�5?A���A�A�bNA���A��/A��FA�hsA��HA�^5A���A�XA��\A��HA��wA�7LA��HA�?}A��DA�\)A�?}A�/A�A|bAw��Au�AtbAr5?Ap �An�AljAj1AhbAf�`Af��AfbAcdZA`bNA_oA^bNA]�A\ �AZ�RAXr�AW�7AU`BAT5?AR��APz�ANA�AM�hAMO�AL�9AI��AF��AEXAB�+A@E�A=XA9��A8I�A7�hA7"�A7oA6�A6ȴA6bNA6^5A6^5A6I�A6bA5�wA5`BA3�
A/��A/p�A.ĜA-A-&�A,��A,I�A,1A+dZA)�A(�DA&��A%��A%��A%K�A$�+A$  A#XA!`BA �RA �+A Q�A�AE�A��Ap�A�+At�A��At�A�`AĜA�A��A�Ap�A��AbNA��A��A=qAp�Av�Ap�A��AZA�AC�A{AK�A
�RA
I�A	l�AQ�AG�AA�A/Az�AbA��AdZA�jA{@��P@�$�@�l�@��@�V@�7L@�r�@�l�@��T@��@�D@�
=@�ff@�J@�Ĝ@�P@�-@�Ĝ@�K�@��#@��;@�@��H@�-@�1'@��@ف@�?}@�Ĝ@�r�@��H@ԓu@���@ӍP@ҏ\@�%@Ͼw@Χ�@̋D@�$�@Ɂ@�bN@�\)@Ɵ�@�O�@�r�@�dZ@���@�@�E�@���@��;@�M�@��h@��@��w@�hs@��@�Z@���@�\)@��y@���@�n�@��+@�33@�C�@�=q@���@���@�j@���@�K�@��+@�-@���@���@�ƨ@�|�@�S�@��@��@�E�@��@���@��#@�=q@��^@�Ĝ@�bN@���@�+@��R@���@��\@��\@�n�@���@�ff@��#@��/@�r�@�I�@���@�33@��@���@�o@�|�@�|�@��H@�ȴ@�n�@�M�@��#@�x�@�&�@���@�Q�@�(�@�9X@� �@�1'@��w@�l�@�\)@�S�@�K�@�S�@�\)@�|�@���@��@��w@�|�@�K�@��@�@��H@�@���@�X@���@��`@���@��@��m@�C�@�~�@�V@�J@���@�p�@�O�@�%@��D@�r�@�j@�I�@� �@��m@�t�@��y@��R@�-@�J@���@��^@�p�@�&�@��9@�bN@��@��@��@�l�@�K�@�+@��y@�v�@��#@�X@��@��@��D@�j@��@�C�@�+@��y@�n�@�5?@�J@��@���@�X@�%@��@��j@��@�  @��@�K�@�33@�
=@���@���@���@�^5@�$�@�$�@�{@�@��@��7@���@���@��@�\)@�+@�C�@�o@��R@��R@�@��@���@��\@�n�@�@���@��^@���@��h@�`B@��@��@��@���@��@��@�33@��!@�5?@��^@���@�(�@�1@��@���@��w@��P@�@��@���@��R@���@�^5@�{@���@���@���@�x�@�`B@�&�@��@��@��@��@��u@�  @�w@�@+@~�y@~E�@~{@}�T@}@}�-@}�@}p�@}/@|j@{�m@{�
@|Z@}?}@}O�@}�@|�@|��@{��@{"�@z�@z��@z-@zJ@y�@y��@yX@xĜ@xbN@x  @w\)@w�@v�R@vff@vE�@v{@uV@tz�@s�
@sS�@r��@rM�@r-@q�@q�#@rJ@q�@q��@q�@q��@p��@pb@o�@n��@n��@n5?@m�T@m@m@o+@ol�@o\)@nE�@l�/@n�R@oK�@ol�@oK�@o�@nff@m�T@m@m��@m�@mV@l�j@l��@lZ@kƨ@kdZ@j��@i��@i�#@i��@i�7@h�9@hr�@hQ�@h  @g�P@f�R@e�@e�h@e�@eO�@d��@d��@d9X@c�
@cC�@b^5@a�@a�^@a��@ahs@aG�@aG�@a7L@a%@`Q�@_��@_l�@_K�@^�R@^V@^@]�-@]p�@]O�@]?}@]�@\��@\��@\�/@\�@\��@\I�@\�@[��@[dZ@[S�@[@Zn�@Y�^@Y��@Yhs@YG�@Y%@XĜ@X�u@X�@XbN@W�@W�P@Wl�@WK�@V�@V��@V5?@U@Up�@U?}@U�@Tz�@T9X@T(�@T1@S�
@S"�@R��@Rn�@R^5@RM�@RJ@Q�7@QG�@Q%@P�@Pr�@P  @O;d@N�+@Nff@N$�@M�T@Mp�@MO�@L�@L�@K�m@K�m@K�F@K�F@K��@K33@J�!@Jn�@JM�@JJ@IG�@H�@H �@Hb@G�@G�w@G�w@G�w@Gl�@Fff@F$�@E�@FV@F$�@D�@D��@D��@E�@E?}@E�@D�j@Cƨ@Co@B�H@B=q@A�@A�^@A�@@�9@@bN@?�@?��@?\)@?�@>�@>��@>�+@>v�@>v�@>V@>$�@>{@=�h@<�/@<�D@<(�@;�
@;dZ@;33@;33@;dZ@;dZ@;"�@:��@:��@:�\@9�#@9x�@9%@8�9@8�@7�@7K�@7�@7�@7
=@6��@6��@6ff@65?@5�-@5�@5O�@4�@4�j@3�F@3t�@2�@2�\@2~�@2~�@2n�@2M�@2J@1��@1�^@1��@1��@17L@0��@0�9@0bN@0A�@0  @/�@/l�@/K�@/�@.��@.��@.�+@.ff@.5?@.5?@.$�@.@-�@-��@,��@,Z@,9X@,1@+�F@+t�@+C�@+o@*��@*^5@*J@)�@)��@)��@)X@(��@(�9@(��@(r�@(bN@(1'@(  @'�w@'\)@'
=@&v�@%�@%�h@%�@$��@$�/@$�D@$j@$Z@$(�@#�m@#ƨ@#ƨ@#��@#dZ@#dZ@#"�@"�@"��@"��@"~�@"=q@"�@!�@!�7@!X@!7L@ �`@ ��@ Ĝ@ �9@ ��@ �u@ bN@  �@�;@�;@�w@�@�@��@��@�P@�P@\)@K�@��@ȴ@�R@��@ff@V@5?@$�@{@@�T@�h@`B@O�@/@�/@�@I�@9X@�@��@�F@��@C�@�H@��@=q@��@�@��@�7@X@7L@�@�`@�9@��@�@Q�@A�@1'@  @�@�@�@\)@K�@�@�y@��@v�@V@E�@5?@5?@@@�-@�-@��@��@�h@�h@�@�@p�@?}@��@�@�j@z�@9X@��@�@t�@"�@@��@n�@-@�@��@��@x�@G�@%@Ĝ@�9@�u@�@bN@1'@b@�;@�w@�w@�@�P@
=@�y@�@ȴ@��@�+@V@{@�T@�T@�-@�@p�@/@�/@��@��@��@9X@��@�m@�
@S�@C�@33@
��@
~�@
M�@
�@	��@	�@	�#1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�C�A�;dA�A�A�K�A�K�A�G�A�K�A�VA�S�A�S�A�XA�S�A�G�A�1AЬA�I�A�+A�A�bNA��A�dZA�;dA�%A���A�7LA�ZA��DA�bNA�O�A�=qA�{A��/A�ĜA�I�A�1'A���A��hA�\)A�ȴA�^5A���A���A��\A�ffA�A�A�`BA�^5A�ƨA�t�A�+A��7A��A�\)A���A��jA�33A��mA��RA���A�M�A�%A�ZA�^5A��A��A��^A��7A�K�A�VA��#A�A�p�A���A�|�A�7LA��A��TA���A�hsA�G�A� �A���A��PA���A� �A�{A�ȴA��A�5?A���A�A�bNA���A��/A��FA�hsA��HA�^5A���A�XA��\A��HA��wA�7LA��HA�?}A��DA�\)A�?}A�/A�A|bAw��Au�AtbAr5?Ap �An�AljAj1AhbAf�`Af��AfbAcdZA`bNA_oA^bNA]�A\ �AZ�RAXr�AW�7AU`BAT5?AR��APz�ANA�AM�hAMO�AL�9AI��AF��AEXAB�+A@E�A=XA9��A8I�A7�hA7"�A7oA6�A6ȴA6bNA6^5A6^5A6I�A6bA5�wA5`BA3�
A/��A/p�A.ĜA-A-&�A,��A,I�A,1A+dZA)�A(�DA&��A%��A%��A%K�A$�+A$  A#XA!`BA �RA �+A Q�A�AE�A��Ap�A�+At�A��At�A�`AĜA�A��A�Ap�A��AbNA��A��A=qAp�Av�Ap�A��AZA�AC�A{AK�A
�RA
I�A	l�AQ�AG�AA�A/Az�AbA��AdZA�jA{@��P@�$�@�l�@��@�V@�7L@�r�@�l�@��T@��@�D@�
=@�ff@�J@�Ĝ@�P@�-@�Ĝ@�K�@��#@��;@�@��H@�-@�1'@��@ف@�?}@�Ĝ@�r�@��H@ԓu@���@ӍP@ҏ\@�%@Ͼw@Χ�@̋D@�$�@Ɂ@�bN@�\)@Ɵ�@�O�@�r�@�dZ@���@�@�E�@���@��;@�M�@��h@��@��w@�hs@��@�Z@���@�\)@��y@���@�n�@��+@�33@�C�@�=q@���@���@�j@���@�K�@��+@�-@���@���@�ƨ@�|�@�S�@��@��@�E�@��@���@��#@�=q@��^@�Ĝ@�bN@���@�+@��R@���@��\@��\@�n�@���@�ff@��#@��/@�r�@�I�@���@�33@��@���@�o@�|�@�|�@��H@�ȴ@�n�@�M�@��#@�x�@�&�@���@�Q�@�(�@�9X@� �@�1'@��w@�l�@�\)@�S�@�K�@�S�@�\)@�|�@���@��@��w@�|�@�K�@��@�@��H@�@���@�X@���@��`@���@��@��m@�C�@�~�@�V@�J@���@�p�@�O�@�%@��D@�r�@�j@�I�@� �@��m@�t�@��y@��R@�-@�J@���@��^@�p�@�&�@��9@�bN@��@��@��@�l�@�K�@�+@��y@�v�@��#@�X@��@��@��D@�j@��@�C�@�+@��y@�n�@�5?@�J@��@���@�X@�%@��@��j@��@�  @��@�K�@�33@�
=@���@���@���@�^5@�$�@�$�@�{@�@��@��7@���@���@��@�\)@�+@�C�@�o@��R@��R@�@��@���@��\@�n�@�@���@��^@���@��h@�`B@��@��@��@���@��@��@�33@��!@�5?@��^@���@�(�@�1@��@���@��w@��P@�@��@���@��R@���@�^5@�{@���@���@���@�x�@�`B@�&�@��@��@��@��@��u@�  @�w@�@+@~�y@~E�@~{@}�T@}@}�-@}�@}p�@}/@|j@{�m@{�
@|Z@}?}@}O�@}�@|�@|��@{��@{"�@z�@z��@z-@zJ@y�@y��@yX@xĜ@xbN@x  @w\)@w�@v�R@vff@vE�@v{@uV@tz�@s�
@sS�@r��@rM�@r-@q�@q�#@rJ@q�@q��@q�@q��@p��@pb@o�@n��@n��@n5?@m�T@m@m@o+@ol�@o\)@nE�@l�/@n�R@oK�@ol�@oK�@o�@nff@m�T@m@m��@m�@mV@l�j@l��@lZ@kƨ@kdZ@j��@i��@i�#@i��@i�7@h�9@hr�@hQ�@h  @g�P@f�R@e�@e�h@e�@eO�@d��@d��@d9X@c�
@cC�@b^5@a�@a�^@a��@ahs@aG�@aG�@a7L@a%@`Q�@_��@_l�@_K�@^�R@^V@^@]�-@]p�@]O�@]?}@]�@\��@\��@\�/@\�@\��@\I�@\�@[��@[dZ@[S�@[@Zn�@Y�^@Y��@Yhs@YG�@Y%@XĜ@X�u@X�@XbN@W�@W�P@Wl�@WK�@V�@V��@V5?@U@Up�@U?}@U�@Tz�@T9X@T(�@T1@S�
@S"�@R��@Rn�@R^5@RM�@RJ@Q�7@QG�@Q%@P�@Pr�@P  @O;d@N�+@Nff@N$�@M�T@Mp�@MO�@L�@L�@K�m@K�m@K�F@K�F@K��@K33@J�!@Jn�@JM�@JJ@IG�@H�@H �@Hb@G�@G�w@G�w@G�w@Gl�@Fff@F$�@E�@FV@F$�@D�@D��@D��@E�@E?}@E�@D�j@Cƨ@Co@B�H@B=q@A�@A�^@A�@@�9@@bN@?�@?��@?\)@?�@>�@>��@>�+@>v�@>v�@>V@>$�@>{@=�h@<�/@<�D@<(�@;�
@;dZ@;33@;33@;dZ@;dZ@;"�@:��@:��@:�\@9�#@9x�@9%@8�9@8�@7�@7K�@7�@7�@7
=@6��@6��@6ff@65?@5�-@5�@5O�@4�@4�j@3�F@3t�@2�@2�\@2~�@2~�@2n�@2M�@2J@1��@1�^@1��@1��@17L@0��@0�9@0bN@0A�@0  @/�@/l�@/K�@/�@.��@.��@.�+@.ff@.5?@.5?@.$�@.@-�@-��@,��@,Z@,9X@,1@+�F@+t�@+C�@+o@*��@*^5@*J@)�@)��@)��@)X@(��@(�9@(��@(r�@(bN@(1'@(  @'�w@'\)@'
=@&v�@%�@%�h@%�@$��@$�/@$�D@$j@$Z@$(�@#�m@#ƨ@#ƨ@#��@#dZ@#dZ@#"�@"�@"��@"��@"~�@"=q@"�@!�@!�7@!X@!7L@ �`@ ��@ Ĝ@ �9@ ��@ �u@ bN@  �@�;@�;@�w@�@�@��@��@�P@�P@\)@K�@��@ȴ@�R@��@ff@V@5?@$�@{@@�T@�h@`B@O�@/@�/@�@I�@9X@�@��@�F@��@C�@�H@��@=q@��@�@��@�7@X@7L@�@�`@�9@��@�@Q�@A�@1'@  @�@�@�@\)@K�@�@�y@��@v�@V@E�@5?@5?@@@�-@�-@��@��@�h@�h@�@�@p�@?}@��@�@�j@z�@9X@��@�@t�@"�@@��@n�@-@�@��@��@x�@G�@%@Ĝ@�9@�u@�@bN@1'@b@�;@�w@�w@�@�P@
=@�y@�@ȴ@��@�+@V@{@�T@�T@�-@�@p�@/@�/@��@��@��@9X@��@�m@�
@S�@C�@33@
��@
~�@
M�@
�@	��@	�@	�#1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B��B��B��B��B��B��B��B��BB+BR�B��BF�BR�B\)B�+B��B�-B��B��BǮBǮB��B��B��B��B��B��B��BƨBĜB��B��B��B�}B�9B�-B�B��B��B��B��B��B��B��B��B�hB�PB�PB�PB�Br�Bo�Bm�Bk�BiyBe`BW
B7LB�B�BVBDB+BB��B�;B��B�RB�B��B��B��B��B��B��B�BffB]/BW
BB�B�B%B��B��B�B�NB�
BȴB�RB�B�B��B��B~�B`BB49B�B
��B
�B
��B
ǮB
�qB
�!B
��B
�1B
hsB
R�B
&�B
B	�B	�B	�;B	��B	ĜB	�?B	��B	��B	�oB	�\B	�=B	z�B	hsB	aHB	\)B	T�B	M�B	E�B	8RB	1'B	&�B	�B	{B	B��B�B�B�ZB�B��BȴB�jB�?B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�hB�PB�DB�1B�%B�B�B�B�B}�By�Bt�Bq�Bq�Bp�Bm�Bk�BiyBe`BcTBbNBbNBdZBdZBcTBbNBaHB`BB^5B^5B]/B\)B[#BYBXBW
BVBS�BR�BO�BM�BK�BH�BD�BC�BA�B@�B=qB<jB:^B8RB5?B33B1'B-B,B)�B(�B'�B%�B$�B"�B�B�B�BoBbB\BVBPBJBDBJBJBJBPB\BoBuB{B�B�B�B�B�B�B�B �B#�B#�B#�B#�B"�B%�B(�B(�B&�B%�B(�B+B-B2-B7LB7LB6FB49B5?B5?B5?B6FB6FB5?B5?B9XB<jB@�BB�BD�BH�BH�BI�BK�BR�BT�BW
BYB`BBe`Bm�Bp�Bm�Bk�Bs�Bx�B}�B� B� B~�B~�B�B�B�JB�\B�bB�{B��B��B��B��B��B��B��B�3B�qB��BŢBŢBɺB��B��B��B�B�
B��B�B�#B�/B�ZB�mB�sB�B��B��B��B��B��B��B	B	B	%B	DB	VB	oB	�B	�B	�B	%�B	&�B	'�B	(�B	)�B	-B	0!B	2-B	6FB	8RB	<jB	?}B	A�B	B�B	D�B	E�B	F�B	K�B	O�B	P�B	R�B	S�B	S�B	VB	W
B	VB	\)B	`BB	aHB	bNB	cTB	dZB	hsB	hsB	iyB	iyB	jB	k�B	m�B	p�B	s�B	v�B	w�B	x�B	}�B	�B	�B	�B	�1B	�DB	�PB	�bB	�hB	�hB	�hB	�hB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�'B	�'B	�-B	�-B	�3B	�9B	�?B	�RB	�dB	�dB	�jB	�jB	�jB	�jB	�^B	�XB	�RB	�XB	�dB	�}B	��B	B	ÖB	ƨB	ǮB	ȴB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�B	�
B	�B	�B	�B	�#B	�#B	�)B	�)B	�)B	�5B	�5B	�5B	�;B	�;B	�BB	�BB	�BB	�BB	�HB	�NB	�NB	�NB	�NB	�NB	�NB	�ZB	�`B	�ZB	�ZB	�ZB	�ZB	�ZB	�ZB	�ZB	�fB	�fB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
%B
+B
+B
%B
%B
PB
\B
bB
hB
hB
oB
uB
uB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
!�B
"�B
"�B
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
%�B
%�B
%�B
%�B
%�B
&�B
&�B
&�B
'�B
'�B
'�B
&�B
&�B
'�B
'�B
'�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
)�B
)�B
(�B
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
+B
+B
+B
,B
,B
,B
-B
-B
-B
-B
.B
/B
/B
/B
/B
/B
0!B
0!B
1'B
2-B
33B
33B
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
8RB
8RB
8RB
7LB
7LB
8RB
:^B
:^B
8RB
9XB
:^B
<jB
=qB
=qB
<jB
<jB
<jB
<jB
=qB
=qB
=qB
>wB
?}B
?}B
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
B�B
B�B
C�B
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
E�B
E�B
E�B
F�B
F�B
F�B
G�B
F�B
F�B
F�B
G�B
H�B
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
M�B
M�B
M�B
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
P�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
R�B
R�B
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
XB
YB
YB
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
[#B
[#B
[#B
[#B
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
`BB
`BB
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
cTB
cTB
cTB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
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
ffB
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
iyB
iyB
iyB
iyB
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
m�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
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
s�B
s�B
s�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�+B��B��B�B�B��B��B�B�B�B�)BRB�BS�B��BGBSzB]�B��B�]B��B�B�NB�B�#B�BмB�SB�OBΩB��B�hB��B��B΃B�VB��B�&B�8B��B��B�oB��B��B�B��B��B�[B�(B��B�GB�B�)B�+Bu BqBn}Bl8Bj�Bi�B_�B>B"�B�B;B.BSB'B��B�B֏B�"B��B�CB��B��B�cB��B�OB��BhB^�B[BKB"�B�B�uB��B��B�fB�SB̞B�B��B��B��B�B��Bg�B88B%yB0B
܎B
ЕB
�B
�`B
�B
��B
�JB
l�B
].B
/uB
	�B	��B	�B	�XB	��B	�B	��B	�B	�0B	�?B	��B	�/B	��B	k�B	c B	_-B	WpB	Q?B	KB	:�B	6]B	)�B	"lB	�B	
IB�{B�jB�*B�B��B��B�TB�B��B��B�~B��B�&B�UB�zB�yB�B�(B�"B�WB��B��B�B��B�B�B�GB�B��B��B�3B�'B�ZB�5B�xB\Bw�BrfBsBsBobBm�BoBgyBdBcGBf Bf�BezBe-BeBd�Bb�BaNB_�B]�B^?B]�B[BYVBX0BWsBU�BT�BQlBO�BN(BKBBF.BD�BB�BBcB@�B>�B<B9�B7�B65B4B/�B/B,
B*NB)+B&�B&�B$�B%wB�B7B�B�B�B�B�BwBlBBWBPBB<B=BuB�B�B�B�BBB�B�B$8B$�B$zB$�B$�B%�B)pB*#B)�B(�B(�B+6B,�B0�B6B8�B9fB8B5�B7�B6�B70B71B7 B6B6dB<�B>�BA�BCBF�BK�BI�BJ�BL�BS�BU�BW}BY�B`aBd�Bm�Br"BoXBl(BtABy�B~�B�7B��B�B�B�MB��B��B��B��B��B�VB�BB��B��B��B�uB��B�8B��B�cB��B�B��B�9B��BӅB�B��B��BֆB�B�LB�B��B�B�=B�	B��B�=B��B�gB��B	�B	�B	�B	>B	�B	�B	�B	�B	 �B	&�B	'BB	(:B	)AB	*0B	-?B	0+B	2:B	6jB	8�B	=B	@B	BB	B�B	EB	G*B	GhB	L�B	P�B	QB	S}B	T�B	TrB	W B	X0B	VwB	\�B	a
B	a�B	b�B	c�B	e4B	h�B	h�B	i�B	i�B	kB	l\B	n�B	q$B	t�B	w4B	x,B	ymB	~�B	��B	��B	��B	��B	�B	��B	��B	��B	��B	�B	�IB	��B	�uB	�!B	�B	�`B	�B	��B	�B	�B	�_B	��B	�KB	�HB	�DB	�YB	��B	��B	�nB	��B	��B	�$B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�KB	��B	��B	�NB	��B	��B	��B	�B	�PB	��B	�iB	�B	�;B	�=B	�,B	ʞB	�CB	�(B	�B	�2B	�\B	˴B	̱B	��B	�?B	�CB	�YB	ФB	��B	��B	�B	�mB	�mB	�yB	�tB	�~B	�mB	ץB	�-B	ؐB	�zB	�mB	ۂB	ܶB	ܼB	ܺB	�}B	ޚB	ްB	ߙB	��B	��B	��B	��B	�B	��B	�CB	�B	�B	��B	�B	��B	�B	�B	�B	�B	�B	�B	��B	�B	��B	�B	�KB	�B	�B	��B	�B	��B	�B	�-B	�B	�B	�+B	��B	��B	��B	�3B	�FB	�"B	�B	�KB	�B	�,B	�B	�B	�B	�B	�UB	�aB	�[B	��B	�HB	�(B	�?B	�B	��B	�>B	�CB	�B	�RB	��B	��B	�KB	��B	�{B	�sB	�lB	�LB	�1B	�FB
8B
�B
@B
NB
B
'B
�B
�B
�B
-B

B
�B
�B
�B
B
�B
�B
�B
;B
B
[B
BB
�B
�B
B
lB
�B
�B
B
9B
�B
kB
&B
�B
B
#B
*B
+B
-B
KB
B
 BB
 B
!B
!%B
"B
"B
"B
"(B
#�B
#eB
$UB
$.B
$zB
$WB
$IB
%QB
%JB
%3B
%)B
%5B
&:B
&#B
&9B
&BB
&/B
'`B
'IB
'zB
(ZB
(<B
(jB
'�B
'�B
(HB
(SB
(HB
)fB
)hB
)XB
)GB
)OB
)�B
)xB
)MB
)SB
)�B
*iB
*�B
)�B
*sB
*`B
*WB
*�B
+oB
+OB
+[B
+iB
+�B
+�B
+eB
+MB
+NB
,|B
,�B
,wB
-�B
-�B
-_B
-�B
.�B
/�B
/xB
/�B
/�B
/�B
0{B
0�B
2B
2�B
3rB
3�B
3wB
3�B
3�B
4�B
5�B
5�B
5�B
6B
7B
6�B
6�B
6�B
6�B
7�B
8�B
8�B
9ZB
7�B
7�B
8JB
:�B
;�B
8�B
9rB
:aB
<�B
=�B
>B
=xB
=8B
<�B
=)B
=�B
=�B
>6B
?B
?�B
@B
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
B�B
C$B
DKB
CB
CB
CB
CB
C�B
C�B
C�B
D�B
FB
F#B
E�B
E�B
FTB
F)B
G1B
GB
G	B
H{B
G,B
GB
F�B
G�B
IB
J<B
JB
JB
JJB
JB
K"B
KAB
K!B
K�B
K,B
KWB
LFB
LB
LB
LB
LB
L2B
LB
M8B
MB
MB
MLB
NAB
N@B
NJB
O0B
ODB
ORB
OCB
O2B
O9B
P:B
PWB
P4B
Q8B
QJB
Q#B
Q/B
Q;B
Q2B
Q`B
Q�B
R�B
REB
RNB
SiB
SaB
TXB
TXB
ToB
T{B
UyB
UTB
URB
URB
UrB
U�B
VrB
VPB
ViB
VPB
VlB
WnB
WxB
W�B
W�B
X�B
X�B
Y�B
Y�B
ZwB
ZwB
Z�B
ZsB
ZlB
Z�B
Z�B
[|B
[dB
[|B
[�B
[gB
[�B
[�B
[�B
\�B
\�B
\�B
\�B
\�B
]�B
]�B
]�B
]�B
]|B
]�B
^�B
^�B
^�B
^�B
^�B
^�B
^vB
^�B
_�B
_|B
_�B
_|B
_�B
_�B
_�B
_�B
_�B
_�B
_�B
_�B
`�B
`�B
`�B
`�B
`�B
`�B
`�B
`�B
`�B
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
c�B
c�B
c�B
d�B
d�B
d�B
d�B
d�B
d�B
d�B
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
f�B
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
i�B
i�B
i�B
i�B
i�B
i�B
i�B
i�B
i�B
jHB
j�B
j�B
k	B
j�B
k B
kB
lB
lB
l�B
l�B
l�B
l�B
mB
nB
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
m�B
o6B
n�B
o�B
o�B
o�B
o�B
pB
pB
p B
p�B
qB
qB
p�B
qB
q"B
q�B
q�B
rB
r4B
rB
r�B
sB
sPB
r�B
r�B
sFB
t(B
tB
tB
tB
s�B
s�B
s�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<S�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<%Jx<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<7#<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<*�9<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - (REPORTED_SURFACE_PRESSURE) for PADJ                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al,2007,JAOT & effects of pressure adjustments                                                                                                                                                            PADJ REPORTED_SURFACE_PRESSURE =0.49 dbar                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CTM alpha = 0.141 & tau = 6.68 s with error equal to the correction                                                                                                                                                                                             Pressures adjusted using reported surface pressure when warranted.                                                                                                                                                                                              none                                                                                                                                                                                                                                                            Salinity corrected for pressure sensor calibration drift and cell thermal mass                                                                                                                                                                                  201603101135232016031011352320160310113523  AO  ARCAADJP                                                                    20140721233944    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721233944  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721233944  QCF$                G�O�G�O�G�O�0                                                                                                                                   G�O�G�O�G�O�                PM  ARSQPADJV1.1                                                                20160310113523  QC  PRES            @�33D�� G�O�                PM  ARSQCTM V1.1                                                                20160310113523  QC  PSAL            @�33D�� G�O�                PM  ARSQOWGUV1.0WOD + Argo                                                      20170523133347  IP                  G�O�G�O�G�O�                