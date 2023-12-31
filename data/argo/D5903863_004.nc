CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       d2014-07-21T23:38:44Z creation; 2014-07-21T23:38:44Z updated; 2015-09-28T12:13:19Z converted from 3.0   
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
resolution        =���   axis      Z        X  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H    PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     X  K�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Y�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     X  ]�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     X  k�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  z8   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     X  }�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �(   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     X  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     X  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �p   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     X  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �`   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     X  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �P   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    Ѐ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    Ӏ   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ր   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ـ   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ٬   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ٰ   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ٴ   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ٸ   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ټ   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �    HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �$   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �(   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �,Argo profile    3.1 1.2 19500101000000  20140721233844  20170523133322  5903863 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               A   AO  4298_0127_004                   2C  D   NAVIS_A                         0127                            120111                          863 @�)��r 1   @�)�9�@@5�O�;dZ�d���S��1   GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                      A   A   A   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��Dy�D��D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�<�D�|�D�� D�  D�@ D� D�� D�  D�@ D�3D��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @n�R@�\)@�\)A�A;�A[�A{�A��
A��
A��
A��
A��
A��
A��
A��
B�B�B�B�B&�B.�B6�B>�BF�BN�BV�B^�Bf�Bn�Bv�B~�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC�ФC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC�ФC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qD n�D �Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�D	n�D	�D
n�D
�Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�Dn�D�RDhRD�RD n�D �D!n�D!�D"n�D"�D#n�D#�D$n�D$�D%n�D%�D&n�D&�D'n�D'�D(n�D(�D)n�D)�D*n�D*�D+n�D+�D,n�D,�D-n�D-�D.n�D.�D/n�D/�D0n�D0�D1n�D1�D2n�D2�D3n�D3�D4n�D4�D5n�D5�D6n�D6�D7n�D7�D8n�D8�D9n�D9�D:n�D:�D;n�D;�D<n�D<�D=n�D=�D>n�D>�D?n�D?�D@n�D@�DAn�DA�DBn�DB�DCn�DC�DDn�DD�DEn�DE�DFn�DF�DGn�DG�DHn�DH�DIn�DI�DJn�DJ�DKn�DK�DLn�DL�DMn�DM�DNn�DN�DOn�DO�DPn�DP�DQn�DQ�DRn�DR�DSn�DS�DTn�DT�DUn�DU�DVn�DV�DWn�DW�DXn�DX�DYn�DY�DZn�DZ�D[n�D[�D\n�D\�D]n�D]�D^n�D^�D_n�D_�D`n�D`�Dan�Da�Dbn�Db�Dcn�Dc�Ddn�Dd�Den�De�Dfn�Df�Dgn�Dg�Dhn�Dh�Din�Di�Djn�Dj�Dkn�Dk�Dln�Dl�Dmn�Dm�Dnn�Dn�Don�Do�Dpn�Dp�Dqn�Dq�Drn�Dr�Dsn�Ds�Dtn�Dt�Dun�Du�Dvn�Dv�Dwn�Dw�Dxn�Dx�Dyn�Dy�Dzn�Dz�D{n�D{�D|n�D|�D}n�D}�D~n�D~�Dn�D�D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��)D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D��\D��\D�7\D�w\D·\D��\D�7\D�w\D÷\D��\D�7\D�w\Dķ\D��\D�7\D�w\Dŷ\D��\D�7\D�w\DƷ\D��\D�7\D�w\DǷ\D��\D�7\D�w\Dȷ\D��\D�7\D�w\Dɷ\D��\D�7\D�w\Dʷ\D��\D�7\D�w\D˷\D��\D�7\D�w\D̷\D��\D�7\D�w\Dͷ\D��\D�7\D�w\Dη\D��\D�7\D�w\DϷ\D��\D�7\D�w\Dз\D��\D�7\D�w\Dѷ\D��\D�7\D�w\Dҷ\D��\D�7\D�w\Dӷ\D��\D�7\D�w\DԷ\D��\D�7\D�w\Dշ\D��\D�7\D�w\Dַ\D��\D�7\D�w\D׷\D��\D�7\D�w\Dط\D��\D�7\D�w\Dٷ\D��\D�7\D�w\Dڷ\D��\D�7\D�w\D۷\D��\D�7\D�w\Dܷ\D��\D�7\D�w\Dݷ\D��\D�7\D�w\D޷\D��\D�7\D�w\D߷\D��\D�7\D�w\D�\D��\D�7\D�w\D�\D��\D�7\D�w\D�\D��\D�4)D�t)D�\D��\D�7\D�w\D�\D��\D�7\D�z�D�)111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�bA�VA�{A��A��A�{A�{A�oA�oA�oA��A�{A��A��A��A�bA�{A�JA���A��mA���AøRAã�AÓuA�VA�&�A��/A�?}A��TA�5?A���A��HA���A��PA�K�A��A���A���A�-A��A�  A�  A�
=A���A�E�A�v�A��!A�r�A�  A��
A�S�A��A���A���A�JA��HA��jA�$�A��;A�jA��mA�A�A�A�G�A���A�%A�K�A���A��!A�
=A��PA�Q�A���A���A�I�A���A���A��A���A��FA��;A���A��A�-A��`A�`BA���A�A��PA���A��
A�7LA��DA���A�|�A���A�ZA���A�I�A��`A��A���A�+A���A�1'A��A�K�A��PA�(�A��mA��-A��7A�(�A���A�
=A��A}��A{�AyO�Ax�DAw�Arn�Ap��Ao�wAn��An1'Am
=AkdZAi��AhjAf��Ad�`Ab�uAa��A`��A]�wAZ~�AY�TAYAY7LAW��AVJATA�AR�+ARAP �AN�yAMl�ALn�AJv�AI�7AIO�AHA�AG�AD��AC��ACl�AC/AB��ABI�AA�-A@��A?��A=�A=G�A;��A9�A9"�A7��A6��A5�hA4��A3ƨA3t�A2�A1��A/��A.{A-t�A,ZA+�A)��A'�FA'VA%�wA#A"�\A"A!t�A ��A $�A�DA�A��AffA;dA�!A�A�AA��A�A�9AjA�-Al�A�
A`BAoAn�A��A�/A  A��A�^AG�A
�/A	�mA	C�A9XA�A�A�DA\)A�A�DA��A �/A �A 1'@�K�@�7L@�
=@��u@���@���@�E�@��@��@�J@�\)@���@�ȴ@陚@�1@�o@旍@�p�@�ƨ@�
=@ᙚ@�b@ݙ�@�G�@�1'@�@�&�@�1@؛�@�Z@�33@�ȴ@���@җ�@щ7@щ7@���@���@�{@˾w@�b@ɲ-@ǶF@��@�@Ĵ9@���@���@�hs@�/@��/@�j@��@��@�n�@�X@�Q�@�^5@�|�@���@�Q�@��;@�S�@��@���@�J@�X@��@��j@��u@���@�V@��u@�r�@�Z@��
@�o@�ff@���@��@�1'@�t�@�l�@�K�@�ȴ@�ff@�5?@�@�O�@��@�Ĝ@�r�@�  @��
@��@�l�@��@���@�V@���@���@��@�7L@���@�I�@�bN@��u@�%@��@�&�@�V@�1@�dZ@���@��H@��!@��R@��R@��+@�-@�=q@��T@��#@��-@��@��@���@��j@��;@�S�@��P@���@��P@�dZ@�33@���@�33@���@�33@��!@�$�@���@�Ĝ@�Z@�A�@�A�@�1@��;@�dZ@�+@��@��!@��+@�~�@��+@��+@�J@�G�@�7L@��@�V@���@���@��9@��@�j@�bN@�Z@�I�@�1@��m@�ƨ@���@�S�@��@��@���@��R@�~�@�E�@��@��h@�V@���@��/@�Ĝ@��u@�z�@�9X@�b@���@�+@���@�V@�v�@�v�@�~�@�M�@��h@��@��@�V@��@���@��D@��D@��@��@��@��@��@�Ĝ@���@�r�@�I�@� �@�  @�dZ@�;d@�33@�;d@�S�@�C�@�+@���@�M�@��@��@�@�`B@�&�@�%@��/@��u@�j@�I�@��m@���@���@�S�@�"�@���@��@��y@�ȴ@���@��+@�{@���@�p�@�?}@�/@��@��`@���@��D@��@��9@�j@�1@��
@��
@�ƨ@��w@��F@��w@��w@���@��@�;d@�
=@��@���@�v�@�E�@�$�@��@��7@�`B@�V@�Ĝ@�bN@�1@�w@|�@;d@
=@~��@~ȴ@~V@}�@}�@|��@|�D@|(�@{ƨ@{t�@z�H@z��@zn�@z=q@z�@y�@x��@xA�@w��@w�P@w\)@w
=@v��@v�+@vV@vE�@v5?@v@u�-@t��@t��@t�D@t�D@tz�@tj@t(�@s�@sC�@r�@q�@p��@pr�@pA�@o��@o|�@o;d@o
=@n�@n�R@nE�@m@mO�@l�j@lz�@lj@k��@k�
@ko@j��@j�\@jJ@i�7@ihs@h�`@g�;@g|�@g\)@f��@f�@f��@fE�@f{@f@e�@e@e�-@e�h@e`B@d��@dZ@ct�@cS�@cC�@cC�@c"�@b��@a�#@a��@ax�@aX@aG�@a7L@`�`@`Ĝ@`�@`bN@`A�@_�@_K�@^��@^�+@^v�@^E�@]�T@]O�@\��@\�@\Z@\1@[�
@[��@[t�@[o@Z�@Z��@Z=q@Y��@Y�@XĜ@XbN@W�w@W|�@W�@V�+@V@U@UO�@U�@T�j@T�@TI�@S�m@S�@SS�@SC�@SC�@S"�@R=q@Q�#@Q��@QG�@Q�@P��@P�`@P��@PA�@O�;@O�@O��@O|�@Ol�@O\)@OK�@N��@N��@N�+@Nv�@NV@N5?@N5?@N@M�T@M�T@M�T@M��@M�-@M�@L�j@LI�@K�F@KS�@J�@J�!@J-@I�@I��@I�@HĜ@H�u@HbN@Hb@G�@G
=@F�@F�R@F�+@E�@E�T@E�T@E@E�h@Ep�@D��@D�@C�F@Co@B��@Bn�@A�@A�^@A��@A&�@@��@@1'@?�w@?;d@>��@>E�@>$�@=�@=�@=�-@=`B@<�@<�D@;��@;��@:�H@:~�@:n�@:M�@:�@9�@9x�@9&�@9�@9�@9%@8Ĝ@8�u@8A�@8 �@7�;@7�w@7��@7��@7|�@7l�@7\)@7+@6�R@6�+@6$�@6@5@5�h@5p�@5O�@5?}@5/@5�@4�@4�j@4I�@3��@3��@333@2�!@2n�@2�@2�@2�@2J@2J@1x�@1�@1%@0��@0��@0�9@0�u@0r�@0Q�@01'@0b@/��@/;d@/
=@.�R@.5?@-@-O�@-�@,��@,�/@,��@,9X@,1@+�
@+ƨ@+��@+��@+��@+t�@+33@*�!@*M�@*�@)��@)��@)�^@)X@)G�@)7L@(��@(��@(��@(�9@(�@(bN@'�@'��@'��@'�P@'l�@';d@&��@&v�@&$�@%��@%��@%p�@%V@$��@$�j@$��@$I�@$�@#��@#�F@#�@"�@"^5@"-@"J@!��@!��@!G�@ ��@ �u@ b@   @�@�P@l�@ȴ@��@v�@ff@5?@{@�-@`B@?}@/@V@�j@z�@j@j@9X@1@��@��@�
@��@t�@C�@33@�H@=q@�@�#@��@hs@��@��@r�@ �@b@b@  @  @�@�;@��@�@|�@+@�@v�@$�@@�@�-@�@O�@/@V@��@V111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�bA�VA�{A��A��A�{A�{A�oA�oA�oA��A�{A��A��A��A�bA�{A�JA���A��mA���AøRAã�AÓuA�VA�&�A��/A�?}A��TA�5?A���A��HA���A��PA�K�A��A���A���A�-A��A�  A�  A�
=A���A�E�A�v�A��!A�r�A�  A��
A�S�A��A���A���A�JA��HA��jA�$�A��;A�jA��mA�A�A�A�G�A���A�%A�K�A���A��!A�
=A��PA�Q�A���A���A�I�A���A���A��A���A��FA��;A���A��A�-A��`A�`BA���A�A��PA���A��
A�7LA��DA���A�|�A���A�ZA���A�I�A��`A��A���A�+A���A�1'A��A�K�A��PA�(�A��mA��-A��7A�(�A���A�
=A��A}��A{�AyO�Ax�DAw�Arn�Ap��Ao�wAn��An1'Am
=AkdZAi��AhjAf��Ad�`Ab�uAa��A`��A]�wAZ~�AY�TAYAY7LAW��AVJATA�AR�+ARAP �AN�yAMl�ALn�AJv�AI�7AIO�AHA�AG�AD��AC��ACl�AC/AB��ABI�AA�-A@��A?��A=�A=G�A;��A9�A9"�A7��A6��A5�hA4��A3ƨA3t�A2�A1��A/��A.{A-t�A,ZA+�A)��A'�FA'VA%�wA#A"�\A"A!t�A ��A $�A�DA�A��AffA;dA�!A�A�AA��A�A�9AjA�-Al�A�
A`BAoAn�A��A�/A  A��A�^AG�A
�/A	�mA	C�A9XA�A�A�DA\)A�A�DA��A �/A �A 1'@�K�@�7L@�
=@��u@���@���@�E�@��@��@�J@�\)@���@�ȴ@陚@�1@�o@旍@�p�@�ƨ@�
=@ᙚ@�b@ݙ�@�G�@�1'@�@�&�@�1@؛�@�Z@�33@�ȴ@���@җ�@щ7@щ7@���@���@�{@˾w@�b@ɲ-@ǶF@��@�@Ĵ9@���@���@�hs@�/@��/@�j@��@��@�n�@�X@�Q�@�^5@�|�@���@�Q�@��;@�S�@��@���@�J@�X@��@��j@��u@���@�V@��u@�r�@�Z@��
@�o@�ff@���@��@�1'@�t�@�l�@�K�@�ȴ@�ff@�5?@�@�O�@��@�Ĝ@�r�@�  @��
@��@�l�@��@���@�V@���@���@��@�7L@���@�I�@�bN@��u@�%@��@�&�@�V@�1@�dZ@���@��H@��!@��R@��R@��+@�-@�=q@��T@��#@��-@��@��@���@��j@��;@�S�@��P@���@��P@�dZ@�33@���@�33@���@�33@��!@�$�@���@�Ĝ@�Z@�A�@�A�@�1@��;@�dZ@�+@��@��!@��+@�~�@��+@��+@�J@�G�@�7L@��@�V@���@���@��9@��@�j@�bN@�Z@�I�@�1@��m@�ƨ@���@�S�@��@��@���@��R@�~�@�E�@��@��h@�V@���@��/@�Ĝ@��u@�z�@�9X@�b@���@�+@���@�V@�v�@�v�@�~�@�M�@��h@��@��@�V@��@���@��D@��D@��@��@��@��@��@�Ĝ@���@�r�@�I�@� �@�  @�dZ@�;d@�33@�;d@�S�@�C�@�+@���@�M�@��@��@�@�`B@�&�@�%@��/@��u@�j@�I�@��m@���@���@�S�@�"�@���@��@��y@�ȴ@���@��+@�{@���@�p�@�?}@�/@��@��`@���@��D@��@��9@�j@�1@��
@��
@�ƨ@��w@��F@��w@��w@���@��@�;d@�
=@��@���@�v�@�E�@�$�@��@��7@�`B@�V@�Ĝ@�bN@�1@�w@|�@;d@
=@~��@~ȴ@~V@}�@}�@|��@|�D@|(�@{ƨ@{t�@z�H@z��@zn�@z=q@z�@y�@x��@xA�@w��@w�P@w\)@w
=@v��@v�+@vV@vE�@v5?@v@u�-@t��@t��@t�D@t�D@tz�@tj@t(�@s�@sC�@r�@q�@p��@pr�@pA�@o��@o|�@o;d@o
=@n�@n�R@nE�@m@mO�@l�j@lz�@lj@k��@k�
@ko@j��@j�\@jJ@i�7@ihs@h�`@g�;@g|�@g\)@f��@f�@f��@fE�@f{@f@e�@e@e�-@e�h@e`B@d��@dZ@ct�@cS�@cC�@cC�@c"�@b��@a�#@a��@ax�@aX@aG�@a7L@`�`@`Ĝ@`�@`bN@`A�@_�@_K�@^��@^�+@^v�@^E�@]�T@]O�@\��@\�@\Z@\1@[�
@[��@[t�@[o@Z�@Z��@Z=q@Y��@Y�@XĜ@XbN@W�w@W|�@W�@V�+@V@U@UO�@U�@T�j@T�@TI�@S�m@S�@SS�@SC�@SC�@S"�@R=q@Q�#@Q��@QG�@Q�@P��@P�`@P��@PA�@O�;@O�@O��@O|�@Ol�@O\)@OK�@N��@N��@N�+@Nv�@NV@N5?@N5?@N@M�T@M�T@M�T@M��@M�-@M�@L�j@LI�@K�F@KS�@J�@J�!@J-@I�@I��@I�@HĜ@H�u@HbN@Hb@G�@G
=@F�@F�R@F�+@E�@E�T@E�T@E@E�h@Ep�@D��@D�@C�F@Co@B��@Bn�@A�@A�^@A��@A&�@@��@@1'@?�w@?;d@>��@>E�@>$�@=�@=�@=�-@=`B@<�@<�D@;��@;��@:�H@:~�@:n�@:M�@:�@9�@9x�@9&�@9�@9�@9%@8Ĝ@8�u@8A�@8 �@7�;@7�w@7��@7��@7|�@7l�@7\)@7+@6�R@6�+@6$�@6@5@5�h@5p�@5O�@5?}@5/@5�@4�@4�j@4I�@3��@3��@333@2�!@2n�@2�@2�@2�@2J@2J@1x�@1�@1%@0��@0��@0�9@0�u@0r�@0Q�@01'@0b@/��@/;d@/
=@.�R@.5?@-@-O�@-�@,��@,�/@,��@,9X@,1@+�
@+ƨ@+��@+��@+��@+t�@+33@*�!@*M�@*�@)��@)��@)�^@)X@)G�@)7L@(��@(��@(��@(�9@(�@(bN@'�@'��@'��@'�P@'l�@';d@&��@&v�@&$�@%��@%��@%p�@%V@$��@$�j@$��@$I�@$�@#��@#�F@#�@"�@"^5@"-@"J@!��@!��@!G�@ ��@ �u@ b@   @�@�P@l�@ȴ@��@v�@ff@5?@{@�-@`B@?}@/@V@�j@z�@j@j@9X@1@��@��@�
@��@t�@C�@33@�H@=q@�@�#@��@hs@��@��@r�@ �@b@b@  @  @�@�;@��@�@|�@+@�@v�@$�@@�@�-@�@O�@/@V@��@V111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B� B~�B}�B~�B~�B~�B}�B{�Bz�Bz�B{�B~�B�B�B�1B�JB�uB�hB�PB�+Bx�Bm�BYBB�B5?B1'B.B&�B�BDB��B�B�yB�yB�yB�yB�`B�/B��B��B�B��B{�Bs�BjBaHBS�B?}B.B'�B%�B�BVBB��B��B��B�`B�NB�BB�5B��BƨB�^B��B��B��B�VB�B{�Bu�Bk�BbNBZBP�BJ�BC�B9XB49B.B$�B�B�BuB+B
�B
�/B
��B
��B
ĜB
�wB
�^B
�LB
�?B
�B
��B
��B
�B
m�B
_;B
P�B
I�B
=qB
 �B
�B
VB
1B
B	��B	�B	�fB	�/B	��B	ŢB	�RB	�9B	�B	��B	�=B	�%B	�B	�B	z�B	p�B	ffB	\)B	W
B	K�B	B�B	:^B	5?B	+B	$�B	#�B	�B	�B	bB	DB		7B	1B	%B	B	B��B��B�B�B�yB�`B�NB�BB�;B�BB�TB�`B�sB�B�yB�ZB�5B�#B�B��BȴB��B�}B�^B�9B�!B�B�B��B��B��B��B��B�oB�\B�PB�DB�=B�7B�1B�1B�+B�B�B�B|�Bx�Bv�Bu�Bs�Bq�Bn�Bl�BjBiyBgmBffBdZBcTBaHB`BB_;B_;B_;B^5B]/B]/B]/B]/B\)BZBVBT�BT�BS�BR�BR�BT�BS�BVBVBVBT�BVBVBVBS�BP�BN�BL�BL�BO�BS�BT�BW
BT�BW
B_;BbNBe`BdZB]/BR�BP�BVBQ�BW
BaHBq�B}�By�Bt�Bw�B~�B�B�B�7B�=B�JB�VB�oB��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�-B�^B�dB�jB��BɺB��B��B��B��B��B�B�B�
B�
B�
B�B�B�#B�/B�5B�;B�BB�HB�NB�ZB�`B�yB�B�B�B�B�B��B��B��B	B	%B	
=B	JB	\B	oB	oB	oB	oB	uB	uB	�B	�B	�B	�B	�B	!�B	%�B	(�B	.B	/B	1'B	2-B	;dB	>wB	>wB	=qB	=qB	=qB	B�B	G�B	J�B	K�B	L�B	K�B	K�B	P�B	VB	W
B	YB	ZB	[#B	_;B	_;B	bNB	e`B	hsB	iyB	jB	k�B	m�B	n�B	o�B	p�B	q�B	t�B	u�B	v�B	v�B	v�B	w�B	x�B	y�B	z�B	z�B	z�B	{�B	}�B	~�B	� B	� B	�B	�B	�B	�B	�7B	�7B	�=B	�7B	�=B	�=B	�DB	�DB	�bB	�oB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�-B	�3B	�9B	�9B	�?B	�RB	�dB	�qB	�}B	��B	��B	ÖB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�)B	�/B	�5B	�BB	�BB	�BB	�TB	�`B	�fB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
%B
%B
+B
1B
	7B

=B

=B
DB
JB
PB
PB
VB
VB
VB
VB
VB
\B
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
{B
{B
�B
�B
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
�B
�B
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
%�B
%�B
%�B
&�B
&�B
&�B
&�B
&�B
'�B
'�B
'�B
(�B
(�B
(�B
(�B
)�B
)�B
)�B
)�B
+B
+B
,B
,B
,B
,B
-B
-B
-B
-B
-B
-B
.B
/B
/B
.B
/B
/B
/B
/B
0!B
0!B
1'B
1'B
2-B
2-B
2-B
2-B
2-B
2-B
2-B
2-B
2-B
33B
33B
33B
33B
33B
33B
33B
33B
49B
49B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
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
7LB
7LB
8RB
8RB
9XB
9XB
:^B
:^B
:^B
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
>wB
>wB
>wB
>wB
>wB
>wB
>wB
?}B
?}B
@�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
C�B
C�B
D�B
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
K�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
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
O�B
O�B
O�B
O�B
O�B
O�B
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
S�B
S�B
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
ZB
ZB
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
]/B
]/B
]/B
^5B
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
cTB
cTB
cTB
cTB
dZB
dZB
dZB
dZB
dZB
e`B
e`B
ffB
ffB
ffB
ffB
ffB
ffB
gmB
ffB
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
iy111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�3B�B�!B�*B�0B�*B�3B�&B�&B�B�0B�!B�B�0B�KB�B�AB�sB�RBTB~jBUBJB�B~�B}B}B|AB~�B�B��B��B�BB�pB�AB�9B��B�4Bz]Bq�Ba�BF�B7B2�B1�B.fB$JBvB�SB�:B�B�B�ZB��B�tB�8BոB�[B��B��B~�Bx�Bm�BgB\|BF`B0;B(�B(�B#�B'BsB�pB��B��B� B��B��B�{B�PB�yB�0B��B�B��B��B�B~TBz�BojBe�B]�BS�BM�BHB;,B6�B2(B'8B"%B�B�B�B
��B
��B
�PB
ҔB
ơB
��B
�`B
�"B
��B
��B
�LB
�-B
�OB
q�B
c�B
R�B
L�B
F�B
$IB
�B
pB
	�B
�B	�B	�MB	�B	�YB	ԮB	��B	��B	��B	�(B	��B	��B	��B	�rB	��B	.B	t�B	j7B	]�B	[fB	N�B	F.B	<�B	9�B	-wB	%�B	&�B	!B	�B	�B	�B	
B		aB	�B	B	�B	&B�B�B��B�B��B��B�XB�hB�B�VB�zB�1B�?B��B�{B�B�,B�B�2B�|B�|B�B��B�\B��B��B��B�^B�5B��B�B�`B��B� B�9B�VB��B��B��B��B�NB��B�`B��B��BzBx�Bx{BvnBt�BsBo�BlBkBj�Bh�Bg�BgWBc�Bd�BbiB`
B`�B_�B_�B^"B^B^�B^�B\�BX�BV8BU�BT�BUBW�BW�BV�BV�BVWBWPBV�BW.BV�BW{BVBQ�BP�BN�BO�BPlBU�BV�BY�BV�BVyB_�Bc�Bf%Bf�B`�BTuBQ9B[mBWJBV�B_6Bq~B�LB|�BvBypB��B��B��B��B��B��B�B��B�bB�[B�B��B�B�B�B��B��B��B��B�oB��B�B��B�zB�|B�CB�KB��B��B��B�OB��B��BͤB�RB�{B�BB�+B�eB�B��B׉B��B��B٘B��B��B�BߢB�B��B��B�B�)B�]B�B��B�KB�B�]B��B��B�dB	B	4B	
�B	�B	4B	B	�B	�B	�B	�B	�B	B	�B	,B	�B	
B	"-B	&yB	)CB	.�B	0MB	1�B	2B	;B	>�B	>�B	=�B	=�B	=GB	B.B	HeB	K�B	L�B	MtB	M\B	L~B	Q)B	V)B	W�B	YxB	Z�B	[�B	_�B	_�B	b�B	e�B	h�B	i�B	kfB	l�B	m�B	n�B	o�B	p�B	rB	uB	v2B	wB	v�B	v�B	xB	y[B	z/B	{5B	{JB	{iB	|]B	~SB	MB	�KB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�!B	�0B	�hB	��B	�uB	��B	��B	�+B	��B	��B	�B	�B	�AB	�LB	��B	�HB	�]B	�UB	�\B	�YB	�&B	�PB	��B	��B	��B	��B	��B	ĖB	�B	��B	��B	��B	�B	�"B	�wB	ҦB	�OB	�QB	�hB	ةB	څB	�sB	݇B	ްB	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�TB	�fB	��B	�B	��B	��B	�B	�5B	��B	��B	�B	�\B	��B	�<B	��B	�B	�B	�B	��B	�B	�3B	�GB	��B	�aB	�EB	�dB	��B
 uB
aB
�B
�B
yB
�B
�B
�B
�B
�B
B
|B
lB
TB
vB
�B
�B
�B
	�B

�B

�B
�B
�B
�B
�B
�B
�B
�B
�B
QB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
SB
�B
�B
�B
�B
�B
�B
'B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
�B
�B
B
B
B
0B
�B
�B
!B
�B
]B
 B
 B
2B
/B
�B
 :B
 �B
")B
"B
"+B
#B
#"B
#+B
$B
$B
$B
$B
$B
%B
%#B
%EB
%tB
%�B
&B
&B
&B
&B
&QB
&�B
&)B
&,B
'"B
'B
'B
'EB
'$B
(BB
(*B
(*B
)TB
)�B
)ZB
)mB
*,B
*HB
*oB
*�B
+fB
+cB
,gB
,jB
,RB
,PB
-bB
-~B
-OB
-vB
-�B
-�B
.�B
/�B
/�B
.�B
/tB
/�B
/�B
/�B
0~B
0�B
1nB
1�B
2]B
2�B
2�B
2�B
2pB
2XB
2TB
2eB
2�B
3�B
3B
3�B
3xB
3kB
3aB
3~B
3�B
4�B
4{B
5nB
5zB
5jB
5qB
5jB
5�B
5�B
6�B
6xB
6�B
6B
7rB
7�B
7�B
7pB
7lB
7zB
7�B
7�B
8�B
8�B
9�B
9�B
:�B
:�B
:�B
;�B
;�B
;�B
<�B
<�B
<�B
<�B
<�B
>B
=�B
=�B
=�B
>�B
>�B
>�B
>�B
>�B
>�B
>�B
?�B
@>B
AB
A�B
A�B
B B
B�B
B�B
CB
B�B
CB
DB
DB
E"B
D�B
E�B
E�B
E�B
E�B
E�B
FB
GB
G+B
GB
HKB
HB
H�B
H�B
H�B
H�B
I'B
IB
I�B
I�B
I�B
JB
I�B
JB
I�B
JB
J�B
J�B
J�B
K B
J�B
J�B
KB
K;B
KB
L4B
LB
LB
LB
M	B
MB
L�B
L�B
L�B
MB
MB
M0B
N'B
N)B
N;B
NGB
O&B
O,B
N�B
N�B
OB
N�B
OXB
P>B
PB
PB
PB
PB
PB
QB
QB
QB
Q!B
QXB
QMB
R3B
RKB
RjB
RbB
SeB
S7B
S-B
S,B
SHB
TbB
T>B
T>B
T&B
T/B
T%B
TB
T6B
TQB
U�B
UkB
VMB
V?B
V+B
VYB
VkB
W=B
W6B
W[B
WFB
W/B
WHB
WRB
WIB
W�B
XOB
X\B
X>B
XIB
X\B
XlB
X�B
YvB
YyB
YdB
Y_B
Z�B
ZrB
ZQB
Z]B
Z�B
[lB
[cB
[{B
[oB
[�B
\�B
\wB
\jB
\B
]}B
]�B
]�B
]�B
]�B
^jB
_�B
_{B
_�B
_�B
`�B
`~B
`sB
`�B
`�B
`�B
a�B
a�B
ayB
a�B
a�B
b�B
b~B
bpB
b�B
b�B
c�B
cxB
c�B
c�B
c�B
c�B
c�B
c�B
dB
d�B
d�B
d�B
d�B
d�B
e�B
e�B
f�B
f�B
f�B
f�B
f�B
f�B
g�B
f�B
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
i�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<C}�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<,�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - (REPORTED_SURFACE_PRESSURE) for PADJ                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al,2007,JAOT & effects of pressure adjustments                                                                                                                                                            PADJ REPORTED_SURFACE_PRESSURE =0.27 dbar                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CTM alpha = 0.141 & tau = 6.68 s with error equal to the correction                                                                                                                                                                                             Pressures adjusted using reported surface pressure when warranted.                                                                                                                                                                                              none                                                                                                                                                                                                                                                            Salinity corrected for pressure sensor calibration drift and cell thermal mass                                                                                                                                                                                  201603101134352016031011343520160310113435  AO  ARCAADJP                                                                    20140721233844    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721233844  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721233844  QCF$                G�O�G�O�G�O�0                                                                                                                                   G�O�G�O�G�O�                PM  ARSQPADJV1.1                                                                20160310113435  QC  PRES            @�  D��G�O�                PM  ARSQCTM V1.1                                                                20160310113435  QC  PSAL            @�  D��G�O�                PM  ARSQOWGUV1.0WOD + Argo                                                      20170523133322  IP                  G�O�G�O�G�O�                