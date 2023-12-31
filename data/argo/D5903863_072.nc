CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       d2014-07-21T23:39:30Z creation; 2014-07-21T23:39:30Z updated; 2015-09-28T12:13:14Z converted from 3.0   
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
resolution        =���   axis      Z        L  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     L  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     L  _�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     L  o4   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ~�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     L  �T   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     L  �t   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     L  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     L  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �,   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     L  �    	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
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
_FillValue                    �(Argo profile    3.1 1.2 19500101000000  20140721233930  20170523133341  5903863 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               HA   AO  4298_0127_072                   2C  D   NAVIS_A                         0127                            120111                          863 @���ʆ@1   @���zO�@7�����d!���o1   GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                      HA   A   A   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��3D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @z=q@�Q�@�Q�A(�A8(�AX(�Ax(�A�{A�{A�{A�{A�{A�{A�{A�{B
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
�D`�D�D`�D�D`�D�D`�D�D`�D�D`�D�D`�D�D`�D�D`�D�D`�D�D`�D�D`�D�D`�D�D`�D�D`�D�D`�D�D`�D�D`�D�D`�D�D`�D�D`�D�D `�D �D!`�D!�D"`�D"�D#`�D#�D$`�D$�D%`�D%�D&`�D&�D'`�D'�D(`�D(�D)`�D)�D*`�D*�D+`�D+�D,`�D,�D-`�D-�D.`�D.�D/`�D/�D0`�D0�D1`�D1�D2`�D2�D3`�D3�D4`�D4�D5`�D5�D6`�D6�D7`�D7�D8`�D8�D9`�D9�D:`�D:�D;`�D;�D<`�D<�D=`�D=�D>`�D>�D?`�D?�D@`�D@�DA`�DA�DB`�DB�DC`�DC�DD`�DD�DE`�DE�DF`�DF�DG`�DG�DH`�DH�DI`�DI�DJ`�DJ�DK`�DK�DL`�DL�DM`�DM�DN`�DN�DO`�DO�DP`�DP�DQ`�DQ�DR`�DR�DS`�DS�DT`�DT�DU`�DU�DV`�DV�DW`�DW�DX`�DX�DY`�DY�DZ`�DZ�D[`�D[�D\`�D\�D]`�D]�D^`�D^�D_`�D_�D``�D`�Da`�Da�Db`�Db�Dc`�Dc�Dd`�Dd�De`�De�Df`�Df�Dg`�Dg�Dh`�Dh�Di`�Di�Dj`�Dj�Dk`�Dk�Dl`�Dl�Dm`�Dm�Dn`�Dn�Do`�Do�Dp`�Dp�Dq`�Dq�Dr`�Dr�Ds`�Ds�Dt`�Dt�Du`�Du�Dv`�Dv�Dw`�Dw�Dx`�Dx�Dy`�Dy�Dz`�Dz�D{`�D{�D|`�D|�D}`�D}�D~`�D~�D`�D�D�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�3�D�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�3�D�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD��RD��RD�0RD�pRD°RD��RD�0RD�pRDðRD��RD�0RD�pRDİRD��RD�0RD�pRDŰRD��RD�0RD�pRDưRD��RD�0RD�pRDǰRD��RD�0RD�pRDȰRD��RD�0RD�pRDɰRD��RD�0RD�pRDʰRD��RD�0RD�pRD˰RD��RD�0RD�pRD̰RD��RD�0RD�pRDͰRD��RD�0RD�pRDΰRD��RD�0RD�pRDϰRD��RD�0RD�pRDаRD��RD�0RD�pRDѰRD��RD�0RD�pRDҰRD��RD�0RD�pRDӰRD��RD�0RD�pRD԰RD��RD�0RD�pRDհRD��RD�0RD�pRDְRD��RD�0RD�pRDװRD��RD�0RD�pRDذRD��RD�0RD�pRDٰRD��RD�0RD�pRDڰRD��RD�0RD�pRD۰RD��RD�0RD�pRDܰRD��RD�0RD�pRDݰRD��RD�0RD�pRDްRD��RD�0RD�pRD߰RD��RD�0RD�pRD�RD��RD�0RD�pRD�RD��RD�0RD�pRD�RD��RD�0RD�pRD�RD��RD�0RD�pRD�RD��RD�0RD�pRD�RD��RD�0RD�pRD�RD��RD�0RD�pRD�RD��RD�0RD�pRD�RD��RD�0RD�pRD�RD��RD�0RD�pRD�RD��RD�0RD�pRD�RD��RD�0RD�pRD�RD��RD�0RD�pRD��RD��RD�0RD�pRD�RD��RD�0RD�pRD�RD��RD�0RD�pRD�RD��RD�0RD�pRD�RD��RD�0RD�pRD�RD��RD�0RD�pRD�RD��RD�0RD�pRD���D��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�\)A�\)A�ZA�XA�ZA�\)A�\)A�\)A�^5A�`BA�bNA�bNA�`BA�`BA�dZA�jA�jA�hsA�(�A�1A���A�l�A�JAÁA��hA�hsA�n�A��A�=qA�  A���A�/A�O�A�
=A�=qA��A�JA��A�S�A�M�A��PA�1A��A��#A�ȴA�z�A�&�A���A�  A�S�A��DA��-A�1'A�z�A�K�A�E�A�G�A�x�A���A�  A�~�A�"�A��A��A�|�A�
=A�1'A�{A��7A��7A��PA���A�VA��;A�A���A�%A�\)A�\)A��!A�  A�ZA��A�  A�|�A���A��A��DA���A��/A�ȴA�33A�hsA���A��\A��^A��A�=qA��^A�n�A�Q�A��^A��DA�XA��A�A{�
AxZAv��Aut�AtffAr�\Ap��Am�Am\)AmoAl��AlZAl$�Ak�Ak�Aj��Aj�Ai�
AeS�AaƨA_`BA]|�A[�
AZ�yAYG�AW�AVVAT��ASoAQ��AQVAO�mAMK�AL^5AK��AI�wAH1'AG��AG�AF�/AE�ADQ�AB�AB�AAl�A@�RA?�hA>jA=K�A<��A<�\A<�DA<�+A<Q�A<  A:�A9�
A8z�A8 �A7;dA6��A5�A3�A3�7A3"�A2��A1�A0  A/�A-|�A,�DA+��A*�!A*(�A)G�A(~�A'`BA&jA$�A"��A!��A 9XA�A�AĜAjA��A`BA��A�
A�A��AI�A1AƨA�AE�Al�AȴA^5A$�A��AĜA��A�9A�
A1A
��A
jA	�A	�A	A�yA|�A�AS�A�A�A�;A ��A �@�dZ@�7L@�ƨ@��!@�X@�dZ@�n�@���@��
@� �@�
=@���@�j@�1'@�  @���@�F@�t�@�@�z�@�!@�{@��`@�K�@�ff@�@��T@�"�@ؼj@׮@Չ7@ӶF@��H@��@д9@� �@��;@ϥ�@��#@�33@ɑh@ȓu@ǥ�@�S�@�+@���@�G�@�G�@�hs@��@��@�O�@Õ�@��y@�v�@���@�(�@���@�=q@�ƨ@�p�@�b@���@��R@�V@���@�?}@��@�K�@�t�@�\)@�S�@�S�@�"�@���@�J@��@���@���@���@��@��D@�  @�K�@���@�x�@�G�@��@��j@�b@��w@�
=@�^5@���@��@�  @���@��F@�t�@���@�~�@�M�@��#@���@�G�@��`@��@�b@�  @�ƨ@��P@�S�@�C�@�
=@�ȴ@�5?@��#@�hs@�/@��@��@�I�@���@��w@��w@��H@��+@�M�@���@��-@�x�@�V@��@��@�V@���@�b@���@��@��+@��\@�^5@��#@��^@�X@��/@�Z@�(�@�b@�I�@��F@�l�@�t�@�dZ@�t�@��@�|�@��\@���@���@�9X@��@�b@��@��y@�n�@���@���@��@�7L@�%@���@���@��u@�z�@�Z@�A�@� �@�ƨ@���@�dZ@�33@�o@�
=@��R@��+@�^5@�5?@��#@���@�hs@�&�@��`@���@��D@���@��@���@��D@��@��u@��j@�A�@���@���@���@�33@��@�=q@�J@��@�@�@��^@��^@��#@��@��@�@�J@�{@�$�@��@�{@��@��@�@���@���@�p�@�V@��9@�z�@��@�  @��;@��@�dZ@�
=@���@���@�^5@�@��@��@��T@��#@��-@�O�@�/@�V@��u@�A�@�b@��
@��
@��
@�ƨ@���@�K�@�o@��H@�~�@�^5@�=q@��@���@��j@�bN@�1'@�b@�1@�1@�  @�  @�1@�  @�;@�;@�;@��@�@�w@��@l�@~�y@~$�@}�-@|�j@|I�@|(�@|�@{ƨ@{"�@z��@z�@x�@w�@v��@u�-@u�@t��@t(�@tZ@t�@t��@s�F@sC�@r�!@q��@p�`@p�u@pr�@pbN@o�;@o�@o��@o|�@ol�@ol�@o\)@oK�@o;d@o+@n�y@nȴ@n�R@n��@n�+@nE�@n{@n{@m�@m��@m/@lz�@k��@kS�@kC�@kC�@kS�@kC�@k33@j��@j~�@j�@i�7@h��@h��@hA�@hb@g�w@g;d@g�@f�R@f{@e��@e�@dj@c��@c��@c@b��@b-@a��@aX@a&�@a�@`��@`r�@` �@_�;@_�;@_��@_|�@_|�@_l�@^��@^ff@^E�@^{@]p�@\I�@\I�@[�
@[�F@[33@Z�H@Z=q@Y��@Y&�@Y%@X��@X�9@X�u@XbN@W�;@Wl�@W�@V��@VE�@U@T��@T�D@Tz�@SS�@R��@R��@R��@R~�@R~�@R~�@R~�@Rn�@RM�@Q�^@QG�@P��@P�u@Pb@P �@Pr�@PĜ@P��@QG�@Q��@Q��@Q�^@Q�^@Q�^@Q��@Q��@Q&�@P��@PbN@P �@O�w@O\)@N�@NV@M@Mp�@M�@L��@L�/@L��@Lz�@LI�@K��@K�F@K�@Kt�@K33@K@J�H@J^5@I�#@I�^@I��@I�@H��@HĜ@H�u@HbN@H  @G�w@Gl�@Fȴ@F��@F�+@F�+@F{@E@E�h@Ep�@Ep�@Ep�@E�@Dj@C��@CS�@C33@C33@B�@B�!@B��@B~�@B-@A��@A%@@Ĝ@@�9@@r�@@1'@@  @?�;@?��@?��@?;d@>��@=�@<j@:��@:-@9��@9�@9�^@9��@9�7@97L@9%@8Ĝ@8�9@81'@7�w@7K�@7;d@7;d@7\)@7|�@7�@7|�@7
=@6��@6�+@65?@5�@5�T@5�T@5��@5��@5��@5@5��@5�@4Z@3��@3��@3��@3�@3dZ@3C�@3@2�@2�H@2��@2��@2�!@2��@2�\@2n�@2M�@2=q@2-@2J@1�#@1��@1G�@17L@17L@1%@0�u@0A�@0b@0  @0  @/�@/�P@/l�@/l�@/l�@/l�@/|�@/�P@/�P@/l�@/+@.V@.5?@.{@,�/@+t�@+C�@+33@+"�@+@*��@*��@*�!@*�!@*M�@)�@)�^@)x�@)hs@)G�@)&�@)%@)%@(bN@(1'@( �@(  @'��@'|�@';d@&�@&��@&v�@&ff@&V@&{@%��@%�h@%�h@%`B@%/@$�/@$�@$j@$Z@$(�@#��@#�F@#"�@"�@"��@"�!@"�\@"~�@"^5@"J@ ��@ r�@ b@�w@\)@\)@\)@;d@�@
=@��@v�@E�@E�@E�@{@�-@`B@O�@V@�/@�j@��@j@I�@(�@�@�
@��@t�@C�@�H@��@��@�\@n�@^5@^5@�@��@�@�@�#@��@X@7L@�@%@��@1'@b@�;@�P@ff@$�@��@/@�j@z�@Z@I�@9X@�@��@��@�@t�@C�@�@��@M�@��@�@�^@��@��@��@��@��@�7@x�@hs@X@X@X@Ĝ@�@��@ȴ@ȴ@��@�R@��@��@��@��@��@{@�h@�@`B@?}@��@��@Z@�@�
@�F@�F@�@t�@t�@dZ@t�@S�@33@
�@
�\@
^5@
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�\)A�\)A�ZA�XA�ZA�\)A�\)A�\)A�^5A�`BA�bNA�bNA�`BA�`BA�dZA�jA�jA�hsA�(�A�1A���A�l�A�JAÁA��hA�hsA�n�A��A�=qA�  A���A�/A�O�A�
=A�=qA��A�JA��A�S�A�M�A��PA�1A��A��#A�ȴA�z�A�&�A���A�  A�S�A��DA��-A�1'A�z�A�K�A�E�A�G�A�x�A���A�  A�~�A�"�A��A��A�|�A�
=A�1'A�{A��7A��7A��PA���A�VA��;A�A���A�%A�\)A�\)A��!A�  A�ZA��A�  A�|�A���A��A��DA���A��/A�ȴA�33A�hsA���A��\A��^A��A�=qA��^A�n�A�Q�A��^A��DA�XA��A�A{�
AxZAv��Aut�AtffAr�\Ap��Am�Am\)AmoAl��AlZAl$�Ak�Ak�Aj��Aj�Ai�
AeS�AaƨA_`BA]|�A[�
AZ�yAYG�AW�AVVAT��ASoAQ��AQVAO�mAMK�AL^5AK��AI�wAH1'AG��AG�AF�/AE�ADQ�AB�AB�AAl�A@�RA?�hA>jA=K�A<��A<�\A<�DA<�+A<Q�A<  A:�A9�
A8z�A8 �A7;dA6��A5�A3�A3�7A3"�A2��A1�A0  A/�A-|�A,�DA+��A*�!A*(�A)G�A(~�A'`BA&jA$�A"��A!��A 9XA�A�AĜAjA��A`BA��A�
A�A��AI�A1AƨA�AE�Al�AȴA^5A$�A��AĜA��A�9A�
A1A
��A
jA	�A	�A	A�yA|�A�AS�A�A�A�;A ��A �@�dZ@�7L@�ƨ@��!@�X@�dZ@�n�@���@��
@� �@�
=@���@�j@�1'@�  @���@�F@�t�@�@�z�@�!@�{@��`@�K�@�ff@�@��T@�"�@ؼj@׮@Չ7@ӶF@��H@��@д9@� �@��;@ϥ�@��#@�33@ɑh@ȓu@ǥ�@�S�@�+@���@�G�@�G�@�hs@��@��@�O�@Õ�@��y@�v�@���@�(�@���@�=q@�ƨ@�p�@�b@���@��R@�V@���@�?}@��@�K�@�t�@�\)@�S�@�S�@�"�@���@�J@��@���@���@���@��@��D@�  @�K�@���@�x�@�G�@��@��j@�b@��w@�
=@�^5@���@��@�  @���@��F@�t�@���@�~�@�M�@��#@���@�G�@��`@��@�b@�  @�ƨ@��P@�S�@�C�@�
=@�ȴ@�5?@��#@�hs@�/@��@��@�I�@���@��w@��w@��H@��+@�M�@���@��-@�x�@�V@��@��@�V@���@�b@���@��@��+@��\@�^5@��#@��^@�X@��/@�Z@�(�@�b@�I�@��F@�l�@�t�@�dZ@�t�@��@�|�@��\@���@���@�9X@��@�b@��@��y@�n�@���@���@��@�7L@�%@���@���@��u@�z�@�Z@�A�@� �@�ƨ@���@�dZ@�33@�o@�
=@��R@��+@�^5@�5?@��#@���@�hs@�&�@��`@���@��D@���@��@���@��D@��@��u@��j@�A�@���@���@���@�33@��@�=q@�J@��@�@�@��^@��^@��#@��@��@�@�J@�{@�$�@��@�{@��@��@�@���@���@�p�@�V@��9@�z�@��@�  @��;@��@�dZ@�
=@���@���@�^5@�@��@��@��T@��#@��-@�O�@�/@�V@��u@�A�@�b@��
@��
@��
@�ƨ@���@�K�@�o@��H@�~�@�^5@�=q@��@���@��j@�bN@�1'@�b@�1@�1@�  @�  @�1@�  @�;@�;@�;@��@�@�w@��@l�@~�y@~$�@}�-@|�j@|I�@|(�@|�@{ƨ@{"�@z��@z�@x�@w�@v��@u�-@u�@t��@t(�@tZ@t�@t��@s�F@sC�@r�!@q��@p�`@p�u@pr�@pbN@o�;@o�@o��@o|�@ol�@ol�@o\)@oK�@o;d@o+@n�y@nȴ@n�R@n��@n�+@nE�@n{@n{@m�@m��@m/@lz�@k��@kS�@kC�@kC�@kS�@kC�@k33@j��@j~�@j�@i�7@h��@h��@hA�@hb@g�w@g;d@g�@f�R@f{@e��@e�@dj@c��@c��@c@b��@b-@a��@aX@a&�@a�@`��@`r�@` �@_�;@_�;@_��@_|�@_|�@_l�@^��@^ff@^E�@^{@]p�@\I�@\I�@[�
@[�F@[33@Z�H@Z=q@Y��@Y&�@Y%@X��@X�9@X�u@XbN@W�;@Wl�@W�@V��@VE�@U@T��@T�D@Tz�@SS�@R��@R��@R��@R~�@R~�@R~�@R~�@Rn�@RM�@Q�^@QG�@P��@P�u@Pb@P �@Pr�@PĜ@P��@QG�@Q��@Q��@Q�^@Q�^@Q�^@Q��@Q��@Q&�@P��@PbN@P �@O�w@O\)@N�@NV@M@Mp�@M�@L��@L�/@L��@Lz�@LI�@K��@K�F@K�@Kt�@K33@K@J�H@J^5@I�#@I�^@I��@I�@H��@HĜ@H�u@HbN@H  @G�w@Gl�@Fȴ@F��@F�+@F�+@F{@E@E�h@Ep�@Ep�@Ep�@E�@Dj@C��@CS�@C33@C33@B�@B�!@B��@B~�@B-@A��@A%@@Ĝ@@�9@@r�@@1'@@  @?�;@?��@?��@?;d@>��@=�@<j@:��@:-@9��@9�@9�^@9��@9�7@97L@9%@8Ĝ@8�9@81'@7�w@7K�@7;d@7;d@7\)@7|�@7�@7|�@7
=@6��@6�+@65?@5�@5�T@5�T@5��@5��@5��@5@5��@5�@4Z@3��@3��@3��@3�@3dZ@3C�@3@2�@2�H@2��@2��@2�!@2��@2�\@2n�@2M�@2=q@2-@2J@1�#@1��@1G�@17L@17L@1%@0�u@0A�@0b@0  @0  @/�@/�P@/l�@/l�@/l�@/l�@/|�@/�P@/�P@/l�@/+@.V@.5?@.{@,�/@+t�@+C�@+33@+"�@+@*��@*��@*�!@*�!@*M�@)�@)�^@)x�@)hs@)G�@)&�@)%@)%@(bN@(1'@( �@(  @'��@'|�@';d@&�@&��@&v�@&ff@&V@&{@%��@%�h@%�h@%`B@%/@$�/@$�@$j@$Z@$(�@#��@#�F@#"�@"�@"��@"�!@"�\@"~�@"^5@"J@ ��@ r�@ b@�w@\)@\)@\)@;d@�@
=@��@v�@E�@E�@E�@{@�-@`B@O�@V@�/@�j@��@j@I�@(�@�@�
@��@t�@C�@�H@��@��@�\@n�@^5@^5@�@��@�@�@�#@��@X@7L@�@%@��@1'@b@�;@�P@ff@$�@��@/@�j@z�@Z@I�@9X@�@��@��@�@t�@C�@�@��@M�@��@�@�^@��@��@��@��@��@�7@x�@hs@X@X@X@Ĝ@�@��@ȴ@ȴ@��@�R@��@��@��@��@��@{@�h@�@`B@?}@��@��@Z@�@�
@�F@�F@�@t�@t�@dZ@t�@S�@33@
�@
�\@
^5@
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�sB�HB�#B��B�9B�qB�
B�)B�B��B�mB��B�jB��B�bB�bB�uB�{B}�BYBO�BL�BN�BO�BN�BK�BG�B?}B,B!�B�B��B��B�B�#B��B�NB�B�B�mB�ZB�;B�5B�BB�B��B{�BXB:^B �BuBB�B�ZB�BB��BȴB�LB��B��B�{B�+B}�Br�BdZBO�B.B�BVB
��B
�NB
�B
��B
ɺB
�qB
�B
��B
�1B
q�B
P�B
F�B
B�B
>wB
9XB
0!B
�B
+B	��B	��B	�B	�TB	�B	��B	��B	ɺB	ǮB	ŢB	ĜB	B	��B	�qB	�^B	�3B	��B	�VB	�B	{�B	t�B	p�B	iyB	dZB	^5B	W
B	P�B	K�B	G�B	A�B	:^B	6FB	2-B	,B	&�B	#�B	!�B	�B	�B	{B	\B	PB	
=B	+B	B��B��B��B��B��B��B��B�B�B�sB�ZB�HB�;B�)B�B��B��B��B��BȴBĜB��B�jB�RB�FB�3B�'B�B�B��B��B��B��B��B�\B�=B�+B�%B�B�B�B~�Bz�Bv�Bt�Bs�Br�Bq�Bp�Br�Br�Bp�Bo�Bn�Bl�BjBhsBe`BbNB_;B]/B\)BZBXBT�BQ�BP�BP�BQ�BQ�BP�BP�BO�BN�BM�BN�BO�BO�BN�BO�BN�BM�BL�BO�BO�BN�BO�BO�BO�BO�BO�BN�BO�BP�BR�BQ�BR�BR�BR�BQ�BXB[#B^5B_;BbNBdZBe`Be`Be`BffBgmBjBl�Bm�Bq�Bs�Bv�By�By�B}�B�B�DB�hB��B��B��B��B�B�B�B�9B�?B�3B�B��B��B�B�'B�-B�3B�XB��BǮB��B��B�
B�B�)B�5B�;B�;B�;B�;B�;B�BB�BB�HB�NB�`B�fB�fB�mB�yB�B�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B	  B	  B	B	B	B	B	B	%B	+B	
=B	JB	PB	VB	bB	uB	uB	{B	�B	�B	�B	�B	�B	#�B	$�B	&�B	(�B	+B	.B	1'B	2-B	2-B	2-B	2-B	2-B	33B	7LB	:^B	?}B	D�B	E�B	H�B	I�B	J�B	L�B	N�B	O�B	Q�B	T�B	XB	YB	ZB	ZB	]/B	`BB	cTB	gmB	iyB	jB	m�B	p�B	q�B	s�B	s�B	t�B	v�B	w�B	x�B	z�B	{�B	|�B	}�B	}�B	}�B	�B	�B	�%B	�%B	�1B	�7B	�=B	�DB	�PB	�\B	�bB	�hB	�oB	�uB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�!B	�-B	�9B	�9B	�?B	�FB	�RB	�^B	�dB	�jB	�jB	�jB	�qB	�wB	�}B	��B	��B	��B	B	B	B	ÖB	ÖB	ĜB	ĜB	ŢB	ŢB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�)B	�)B	�)B	�/B	�/B	�/B	�/B	�/B	�5B	�5B	�5B	�5B	�;B	�;B	�;B	�BB	�BB	�HB	�TB	�TB	�ZB	�ZB	�ZB	�ZB	�ZB	�`B	�fB	�mB	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
1B
1B
1B
	7B
	7B

=B
DB
DB
DB
JB
JB
VB
\B
bB
bB
bB
bB
bB
bB
hB
hB
oB
oB
oB
oB
uB
uB
uB
uB
uB
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
 �B
 �B
!�B
#�B
$�B
$�B
&�B
'�B
(�B
(�B
(�B
(�B
(�B
(�B
)�B
,B
-B
-B
-B
.B
.B
/B
/B
0!B
0!B
0!B
0!B
0!B
1'B
1'B
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
33B
33B
49B
49B
49B
49B
49B
49B
5?B
5?B
5?B
6FB
5?B
6FB
6FB
6FB
6FB
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
;dB
;dB
;dB
;dB
:^B
9XB
9XB
9XB
9XB
9XB
9XB
8RB
8RB
8RB
8RB
8RB
9XB
9XB
:^B
:^B
:^B
;dB
<jB
<jB
<jB
;dB
<jB
<jB
;dB
<jB
<jB
<jB
<jB
=qB
=qB
=qB
=qB
=qB
>wB
?}B
?}B
?}B
?}B
?}B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
D�B
E�B
E�B
F�B
F�B
F�B
G�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
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
J�B
J�B
J�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
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
N�B
N�B
M�B
N�B
O�B
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
XB
XB
XB
XB
YB
YB
YB
YB
ZB
ZB
ZB
[#B
\)B
\)B
]/B
]/B
]/B
]/B
^5B
^5B
^5B
^5B
^5B
_;B
`BB
`BB
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
cTB
cTB
cTB
cTB
dZB
dZB
e`B
dZB
dZB
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
gmB
gmB
gmB
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
jB
jB
jB
k�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�CB�xB�(B��B�fB�-B��B��B�nB��B��B�6B�B��B�B��B�+B��B�4B��B�B]6BR�BM�BN�BPxBP�BM�BJ�BG�B/�B&HB xB �B��B�5B��B�XB��B�fB��B�`B��BߌB�B�wB�AB�~B�5B�;B[�B>3B$�BKB�B�IB�vB��B��B�B��B�XB��B��B��B��Bv�BkBY�B3B �B4BB
�~B
�xB
�kB
�oB
��B
�GB
�1B
�ZB
|�B
T9B
G�B
C�B
?�B
<�B
8�B
$�B
�B
 HB	��B	�B	��B	�9B	σB	˸B	ʭB	�B	�\B	�gB	��B	��B	�
B	�B	�^B	�}B	�KB	��B	^B	v�B	tB	l{B	hB	a�B	Z�B	T4B	MQB	J�B	G�B	<�B	8bB	7OB	0,B	(�B	%^B	"�B	"�B	 B	hB	�B	oB	vB	
�B	LB	TB��B��B�B�B��B��B�B�B�#B�B�B�B�OB�WB�,B�"B�YB��B̱B�3BřB��B��B��B��B�B��B�aB��B��B��B�B��B��B�|B��B� B�IB��B��B�dBmBzBu�Bt�Bs�Br�Bs�BuBt�Bq�BpmBp4Bn�BmRBk�Bg�Bg0Bb9B^�B]�B[oBY�BZ�BV
BS�BTBR�BR�BS�BS�BQ�BPBP�BP�BQpBQ�BQ�BQFBP-BP`BQ�BQ�BQ�BP�BPhBP[BP_BP>BP�BQBRFBS�BTBS�BUfBT�BT�BW8B\YB^�B`Bb�Be3Be�BgBgOBfYBf�Bg�Bl�Bo�Bo~BsBt�BwgBzKB{�B~�B�VB�[B��B��B��B��B��B��B��B��B�B��B��B�SB��B��B��B��B�%B�2B�yBïBǷB�AB�GB�KBڥB��B�|BߪBߝBߌB��B�|B�B�cB�B�B�oB��B��B�GB�B�LB��B��B��B�?B��B�%B�B�FB��B�cB�9B��B�QB��B��B��B��B�IB��B��B��B	 \B	 �B	�B	B	�B	B	�B	�B	B	B	
B	�B	�B	�B	GB	B	�B	�B	(B	rB	�B	 B	$;B	%�B	( B	)�B	,^B	.�B	1_B	2�B	3CB	2�B	3B	33B	44B	7�B	:�B	?}B	E�B	F9B	H�B	JB	J�B	L�B	O]B	QDB	S!B	VDB	Y0B	Y�B	ZgB	[B	^1B	a$B	dTB	g�B	i�B	kB	nB	q,B	r"B	tB	tB	u*B	w)B	x=B	y�B	{\B	|wB	}rB	~aB	~EB	~�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�jB	�AB	�NB	��B	��B	��B	�]B	�SB	�WB	�B	�3B	�!B	��B	�B	�0B	�*B	�5B	�2B	�/B	�XB	�^B	�XB	�fB	��B	��B	��B	�B	� B	�B	��B	�9B	��B	��B	��B	�%B	�?B	�+B	��B	�,B	�LB	��B	��B	��B	��B	�B	�nB	�B	�B	ƕB	�aB	�KB	�YB	�B	�B	�0B	�fB	φB	рB	�~B	��B	�rB	�qB	ՠB	֦B	�jB	��B	ܢB	ܓB	�sB	�qB	�xB	�mB	�eB	�yB	މB	�sB	�uB	�|B	ߏB	�sB	ߓB	�B	��B	�B	��B	�9B	��B	�B	�B	��B	�B	��B	�5B	��B	�-B	�B	�}B	�UB	�EB	�=B	��B	��B	�B	�B	�{B	��B	��B	��B	�QB	�1B	�*B	�}B	�EB	�2B	�9B	�5B	�(B	�5B	�3B	�6B	�3B	�[B	�BB	�<B	�<B	�JB	�aB	�SB	�4B	�OB	�zB	��B	��B
 �B
zB
YB
KB
EB
VB
XB
�B
xB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	�B
	�B

�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
CB
�B
�B
�B
,B
xB
�B
B
�B
B
�B
4B
2B
B
�B
�B
�B
�B
�B
/B
(B
B
4B
-B
DB
qB
;B
 B
�B
MB
 B
B
B
�B
�B
�B
B
B
 eB
 NB
!AB
!PB
!eB
 �B
!�B
#�B
$�B
$�B
&�B
("B
)-B
)7B
)7B
)@B
)FB
)�B
*�B
,|B
-�B
-�B
-�B
.�B
.�B
/�B
/�B
0�B
0|B
0}B
0rB
0�B
1�B
1�B
1�B
1�B
1vB
2�B
2�B
2�B
2�B
2�B
3�B
3�B
3�B
3�B
4�B
4�B
4�B
4�B
4�B
4�B
5�B
5�B
5�B
6�B
5�B
6�B
6�B
6�B
6�B
7�B
7�B
7�B
9B
8�B
8�B
9�B
9�B
9�B
9�B
9�B
9�B
9�B
:B
:�B
:�B
:�B
:�B
:�B
:�B
;�B
;�B
;�B
<B
<B
<�B
;�B
9�B
9�B
9�B
9�B
9�B
9�B
8�B
8�B
8�B
8�B
8�B
9�B
9�B
:�B
:�B
:�B
;�B
<�B
<�B
<�B
;�B
<�B
<�B
;�B
<�B
<�B
<�B
<�B
=�B
=�B
=�B
=�B
>uB
?)B
?�B
?�B
?�B
?�B
?�B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
B�B
CB
C�B
C�B
C�B
D*B
DB
C�B
C�B
C�B
C�B
E"B
E�B
E�B
F�B
F�B
F�B
G�B
H�B
I
B
I-B
I�B
IB
IB
I�B
I�B
IB
I B
IB
IB
I$B
H�B
IB
H�B
I,B
I,B
IB
IB
H�B
IB
IB
JB
I�B
J[B
JB
KB
KB
K"B
J,B
J!B
J:B
KB
K"B
KB
KB
K+B
L3B
L2B
L
B
L'B
M/B
MGB
M2B
M9B
MB
N8B
N8B
NDB
N}B
N7B
N+B
N)B
N(B
O$B
O4B
NUB
O�B
PB
QqB
QfB
QnB
R,B
R.B
RFB
RFB
R=B
RyB
RRB
RMB
S0B
S.B
SXB
S�B
SoB
TFB
TkB
T_B
TRB
TUB
T`B
TSB
U[B
UKB
UqB
UsB
UWB
UcB
V�B
V]B
V]B
VPB
V\B
VSB
VFB
V{B
V]B
WYB
WJB
WWB
WYB
W�B
WeB
XhB
XfB
X�B
X�B
YqB
Y{B
Y�B
Z;B
Z�B
Z�B
Z�B
[�B
\�B
\B
]|B
]{B
]�B
]�B
^�B
^B
^�B
^�B
^�B
_�B
`�B
`�B
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
c�B
c�B
dB
daB
eB
d�B
e�B
d�B
d�B
e�B
e�B
e�B
e�B
e�B
fB
gB
f�B
f�B
f�B
g�B
g�B
g�B
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
j�B
j�B
j�B
k�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<5l[<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<$�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<.|�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<F��<#�
<#�
<#�
<#�
<#�
<#�
<'�b<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<6&
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - (REPORTED_SURFACE_PRESSURE) for PADJ                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al,2007,JAOT & effects of pressure adjustments                                                                                                                                                            PADJ REPORTED_SURFACE_PRESSURE =0.49 dbar                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CTM alpha = 0.141 & tau = 6.68 s with error equal to the correction                                                                                                                                                                                             Pressures adjusted using reported surface pressure when warranted.                                                                                                                                                                                              none                                                                                                                                                                                                                                                            Salinity corrected for pressure sensor calibration drift and cell thermal mass                                                                                                                                                                                  201603101135122016031011351220160310113512  AO  ARCAADJP                                                                    20140721233930    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721233930  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721233930  QCF$                G�O�G�O�G�O�0                                                                                                                                   G�O�G�O�G�O�                PM  ARSQPADJV1.1                                                                20160310113512  QC  PRES            @���D���G�O�                PM  ARSQCTM V1.1                                                                20160310113512  QC  PSAL            @���D���G�O�                PM  ARSQOWGUV1.0WOD + Argo                                                      20170523133341  IP                  G�O�G�O�G�O�                