CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS      N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-28T23:01:44Z UW 3.1 conversion   
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8`   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~            8l   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8t   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8|   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�   axis      Z           9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�        C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�        M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�        U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�        _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�        i�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�        q�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    y�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�        {�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�        ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �    HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �L   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �\   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �`   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        �p   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        �t   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �x   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �|Argo profile    3.1 1.2 19500101000000  5901072 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               A   AO  20111130143642  20190522121828  1728_5048_017                   2C  D   APEX                            2142                            040306                          846 @�U(�1   @�U(�� @4�^5?|��cR=p��
1   GPS     Primary sampling: mixed [deeper than nominal 990dbar: discrete; nominal 990dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   B   @�  @�  A   A   A@  A`  A���A�  A�33A�  A�  A�  A�  A�  B   B  B  B  B   B(  B/��B7��B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B���B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  C   C  C�C�C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CQ�fCT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D ��D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� DfD� D  Dy�D  D� D  D� D��D� DfD� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D2��D3y�D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D=��D>� D?fD?� D@  D@�fDAfDA�fDB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ�fDK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dg��Dhy�Di  Di� Dj  Dj�fDk  Dky�Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt�fDufDu� Dv  Dv� Dw  D�	�D�,�D���D�ɚD�3D�0 D�� D���D���D�fD�S3D��fD���D�,�Dډ�D�fD�ٚD�3D�Ff11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@l��@�ff@�ffA33A;33A[33A|��A���A���A���A���A͙�Aݙ�A홚A���B��B��B��B��B&��B.ffB6ffB>��BF��BN��BV��B^��Bf��Bn��Bv��B~��B�ffB�ffB�ffB�ffB�ffB�ffB�33B�ffB�ffB�ffB�ffB�ffB�ffB�ffB���B���B�ffB�ffB�33B�ffB�ffB�ffB�ffB�ffB�ffB�33B�ffB�ffB�ffB�ffB�ffB�ffC�3C��C��C�3C	�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C!�3C#�3C%�3C'�3C)�3C+�3C-�3C/�3C1�3C3�3C5�3C7�3C9�3C;�3C=�3C?�3CA�3CC�3CE�3CG�3CI�3CK�3CM�3CO�3CQ��CS�3CU�3CW�3CY�3C[�3C]�3C_�3Ca�3Cc�3Ce�3Cg�3Ci�3Ck�3Cm�3Co�3Cq�3Cs�3Cu�3Cw�3Cy�3C{�3C}�3C�3C�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC��fC��fC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC��fC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚD l�D �fDl�D��Dl�D��Dl�D��Dl�D��Dl�D��Ds3D��Dl�D��Dl�D��D	l�D	��D
l�D
��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D�fDl�D�3Dl�D��DffD��Dl�D��Dl�D�fDl�D�3Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��D l�D ��D!l�D!��D"l�D"��D#l�D#��D$l�D$��D%l�D%��D&l�D&��D'l�D'��D(l�D(��D)l�D)��D*l�D*��D+l�D+��D,l�D,��D-l�D-��D.l�D.��D/l�D/��D0l�D0��D1l�D1��D2l�D2�fD3ffD3��D4l�D4��D5l�D5��D6l�D6��D7l�D7��D8l�D8��D9l�D9��D:l�D:��D;l�D;��D<l�D<��D=l�D=�fD>l�D>�3D?l�D?��D@s3D@�3DAs3DA��DBl�DB��DCl�DC��DDl�DD��DEl�DE��DFl�DF��DGl�DG��DHl�DH��DIl�DI��DJs3DJ��DKl�DK��DLl�DL��DMl�DM��DNl�DN��DOl�DO��DPl�DP��DQl�DQ��DRl�DR��DSl�DS��DTl�DT��DUl�DU��DVl�DV��DWl�DW��DXl�DX��DYl�DY��DZl�DZ��D[l�D[��D\l�D\��D]l�D]��D^l�D^��D_l�D_��D`l�D`��Dal�Da��Dbl�Db��Dcl�Dc��Ddl�Dd��Del�De��Dfl�Df��Dgl�Dg�fDhffDh��Dil�Di��Djs3Dj��DkffDk��Dll�Dl��Dml�Dm��Dnl�Dn��Dol�Do��Dpl�Dp��Dql�Dq��Drl�Dr��Dsl�Ds��Dts3Dt�3Dul�Du��Dvl�Dv��D�  D�#3D��3D�� D�	�D�&fD��fD�� D�� D��D�I�DǼ�D��3D�#3Dڀ D���D�� D�	�D�<�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�
=A�VA�VA�bA�oA�oA�oA�{A�oA�{A�{A�{A�{A��A��A��A��A��A��A� �A��A��A�"�A� �A�&�A�$�A�$�A�&�A�&�A�-A�=qA�I�A�O�A�Q�A�ZA�hsA�z�AŅAŁA�l�A�Q�A�;dA��A�\)AÉ7A��`A� �A���A���A�^5A�`BA��A��
A��yA�VA��A� �A�ffA��/A�v�A���A��A��mA�K�A�A�A��A���A�9XA�A�z�A�p�A�VA�`BA��A��A��wA��RA��A��`A�VA�&�A�Q�A��A�t�A�A��PA�;dA�33A�%A�VA�M�A��-A��HA�?}A��!A���A�Q�A��;A���A�`BA��`A���A�5?A���A�`BA��TA�A�A�{A~�9A|�yA{AzjAy��Ay�-AxJAt$�ArVAp�yAo��Am\)Al1'AkO�Ah�jAg�Ag7LAe�7Ab5?A_��A]x�A[�7AXĜAW��AV��AU�^AT�!ASx�AR-AQ33AO%AN{AM�AJ��AH��AH�jAF��AD��AA�mA?ƨA@VA?��A>-A<�A<  A;�A9�TA8v�A7�wA6��A5\)A4VA3l�A2��A2 �A0�uA/�A.E�A-/A,�A+�7A+�A*��A*ZA)t�A'�7A'?}A'oA&��A&��A%��A$n�A#VA"�/A"�A ��A Q�A��A~�An�A33A�9A�A&�A��A(�A��Av�AE�A|�AjA\)A��A��A�A�AM�A�hA`BA
�HA	�A9XAhsA�RA$�AdZA�RA�wA�+A�A�A;dA 5?@�5?@��w@�$�@��@�bN@��@���@�X@�D@� �@�33@�J@�7@��/@�Z@@���@���@�7L@�Ĝ@�z�@�b@�|�@���@��T@�&�@� �@�
=@���@�1'@�C�@���@ߝ�@�;d@�+@�
=@ް!@���@��@�ȴ@��@���@�(�@�ȴ@�ff@�J@���@���@թ�@ղ-@պ^@պ^@���@��@�{@�M�@��#@�V@ԣ�@���@Ұ!@Ѻ^@��
@�dZ@Η�@�@͡�@͉7@�hs@�G�@�&�@��/@�A�@�33@�ff@��/@�1'@�"�@��@ř�@ŉ7@Ų-@�{@�^5@ư!@�E�@�{@�@š�@őh@�hs@�7L@�%@���@��@���@�Z@��@�@�$�@�J@���@��j@�Z@�(�@�9X@�r�@�1'@�K�@�5?@�V@���@��9@��@��D@�Z@��@��@���@�K�@��y@���@�^5@�5?@��h@��`@��u@�  @���@�@��@�V@��@��@�z�@�I�@�1'@� �@���@���@�ff@���@�p�@��@�V@���@���@�bN@�1'@��;@��@��P@��@�t�@�C�@�
=@���@�v�@�-@���@�p�@���@��D@�Z@�A�@�A�@�b@���@�l�@�S�@�@�@�`B@��@���@�|�@�n�@�{@���@�`B@��@��D@�9X@�(�@��@�1'@��;@�;d@�;d@�l�@�ƨ@�1@��@�b@�ƨ@�K�@�+@�@��y@��@�ȴ@���@��!@�-@��^@�&�@��u@��;@�S�@�33@���@�~�@�ff@�$�@���@�hs@��D@�ƨ@�\)@�"�@���@���@���@�M�@�@��@�7L@�V@��/@���@�z�@��@��@�t�@�"�@�
=@��y@��!@�n�@�E�@���@���@�`B@��@��@�Ĝ@���@�bN@�Q�@�A�@� �@��F@�C�@��+@�O�@�j@��;@��H@��+@��@���@�&�@���@��@��@�|�@��!@�~�@�5?@�J@��@��T@�t�@��!@s��@pQ�@f��@^V@XĜ@Q�@Ko@D��@?��@;"�@5�-@0�9@,��@$�/@K�@&�@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�
=A�VA�VA�bA�oA�oA�oA�{A�oA�{A�{A�{A�{A��A��A��A��A��A��A� �A��A��A�"�A� �A�&�A�$�A�$�A�&�A�&�A�-A�=qA�I�A�O�A�Q�A�ZA�hsA�z�AŅAŁA�l�A�Q�A�;dA��A�\)AÉ7A��`A� �A���A���A�^5A�`BA��A��
A��yA�VA��A� �A�ffA��/A�v�A���A��A��mA�K�A�A�A��A���A�9XA�A�z�A�p�A�VA�`BA��A��A��wA��RA��A��`A�VA�&�A�Q�A��A�t�A�A��PA�;dA�33A�%A�VA�M�A��-A��HA�?}A��!A���A�Q�A��;A���A�`BA��`A���A�5?A���A�`BA��TA�A�A�{A~�9A|�yA{AzjAy��Ay�-AxJAt$�ArVAp�yAo��Am\)Al1'AkO�Ah�jAg�Ag7LAe�7Ab5?A_��A]x�A[�7AXĜAW��AV��AU�^AT�!ASx�AR-AQ33AO%AN{AM�AJ��AH��AH�jAF��AD��AA�mA?ƨA@VA?��A>-A<�A<  A;�A9�TA8v�A7�wA6��A5\)A4VA3l�A2��A2 �A0�uA/�A.E�A-/A,�A+�7A+�A*��A*ZA)t�A'�7A'?}A'oA&��A&��A%��A$n�A#VA"�/A"�A ��A Q�A��A~�An�A33A�9A�A&�A��A(�A��Av�AE�A|�AjA\)A��A��A�A�AM�A�hA`BA
�HA	�A9XAhsA�RA$�AdZA�RA�wA�+A�A�A;dA 5?@�5?@��w@�$�@��@�bN@��@���@�X@�D@� �@�33@�J@�7@��/@�Z@@���@���@�7L@�Ĝ@�z�@�b@�|�@���@��T@�&�@� �@�
=@���@�1'@�C�@���@ߝ�@�;d@�+@�
=@ް!@���@��@�ȴ@��@���@�(�@�ȴ@�ff@�J@���@���@թ�@ղ-@պ^@պ^@���@��@�{@�M�@��#@�V@ԣ�@���@Ұ!@Ѻ^@��
@�dZ@Η�@�@͡�@͉7@�hs@�G�@�&�@��/@�A�@�33@�ff@��/@�1'@�"�@��@ř�@ŉ7@Ų-@�{@�^5@ư!@�E�@�{@�@š�@őh@�hs@�7L@�%@���@��@���@�Z@��@�@�$�@�J@���@��j@�Z@�(�@�9X@�r�@�1'@�K�@�5?@�V@���@��9@��@��D@�Z@��@��@���@�K�@��y@���@�^5@�5?@��h@��`@��u@�  @���@�@��@�V@��@��@�z�@�I�@�1'@� �@���@���@�ff@���@�p�@��@�V@���@���@�bN@�1'@��;@��@��P@��@�t�@�C�@�
=@���@�v�@�-@���@�p�@���@��D@�Z@�A�@�A�@�b@���@�l�@�S�@�@�@�`B@��@���@�|�@�n�@�{@���@�`B@��@��D@�9X@�(�@��@�1'@��;@�;d@�;d@�l�@�ƨ@�1@��@�b@�ƨ@�K�@�+@�@��y@��@�ȴ@���@��!@�-@��^@�&�@��u@��;@�S�@�33@���@�~�@�ff@�$�@���@�hs@��D@�ƨ@�\)@�"�@���@���@���@�M�@�@��@�7L@�V@��/@���@�z�@��@��@�t�@�"�@�
=@��y@��!@�n�@�E�@���@���@�`B@��@��@�Ĝ@���@�bN@�Q�@�A�@� �@��F@�C�@��+@�O�@�j@��;@��H@��+@��@���@�&�@���@��@��@�|�@��!@�~�@�5?@�J@��@��T@�t�@��!@s��@pQ�@f��@^V@XĜ@Q�@Ko@D��@?��@;"�@5�-@0�9@,��@$�/@K�@&�@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBJBDBDBJBDBPBuB�B�B��B!�B&�B/B49B<jBA�BD�BG�BN�B\)Bl�B�DB��B��B��B�B�qBÖB��B��B�wB�wB�XB�9B�?B�!B�B��B��B��B��B��B�hB�1B}�Bw�Bp�BW
BYBT�BVB=qB<jB8RB#�BhB��B�B�NB�B��B�^B��B��B��B�VB�Bu�BgmBZBO�B0!BuB
��B
�BB
��B
�9B
��B
�7B
�%B
l�B
dZB
dZB
W
B
M�B
6FB
49B
2-B
 �B
�B
�B	�B	�B	�B	�NB	��B	ÖB	�jB	�9B	�'B	��B	��B	�\B	� B	l�B	]/B	XB	S�B	M�B	R�B	Q�B	M�B	J�B	C�B	1'B	)�B	!�B	uB	
=B	%B	  B�B�TB�BB	+B	hB��B��B�B�B�fB�B��B�#B��B��B��BĜB�wB�wB�LB�LB�B�B��B��B��B��B��B��B��B��B��B��B�uB�oB�hB�bB�PB�JB�=B�7B�%B�B}�B}�B|�Bz�B|�Bu�Bq�Bp�Bo�Bn�Bk�BiyBhsBdZBcTBbNBdZBiyBjBjBffBaHBbNBdZBiyBu�Bp�BaHB`BBaHB_;B]/B^5BcTB\)BdZBhsBk�Bm�Br�Bs�Bu�Bu�By�B}�B~�B�B�B�B�1B�JB�\B�\B�hB�oB�uB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�?B�RB�jBBǮB��B��B��B��B�5B�B��B	  B	B	B	%B	
=B	
=B	VB	VB	\B	bB	hB	hB	hB	hB	hB	hB	hB	uB	{B	�B	�B	uB	uB	�B	�B	�B	%�B	,B	5?B	<jB	B�B	G�B	H�B	I�B	K�B	M�B	P�B	R�B	VB	XB	[#B	\)B	XB	[#B	]/B	]/B	]/B	\)B	^5B	bNB	k�B	n�B	q�B	w�B	t�B	s�B	x�B	� B	�B	�%B	�1B	�1B	�DB	�PB	�\B	�hB	�hB	�hB	��B	��B	��B	��B	��B	�uB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�B	�B	�B	�B	�B	�!B	�!B	�-B	�3B	�9B	�?B	�?B	�?B	�FB	�LB	�LB	�LB	�LB	�RB	�XB	�XB	�dB	�wB	�}B	��B	��B	�qB	�RB	�LB	�FB	�FB	�FB	�LB	�XB	�^B	�jB	�wB	B	��B	�}B	��B	B	ƨB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	�
B	�#B	�#B	�B	�
B	�
B	�B	�B	�B	�B	�B	�
B	�B	�)B	�)B	�/B	�5B	�BB	�HB	�BB	�HB	�NB	�NB	�NB	�NB	�NB	�NB	�HB	�NB	�TB	�TB	�ZB	�ZB	�ZB	�ZB	�ZB	�`B	�mB	�sB	�sB	�fB	�ZB	�BB	�/B	�#B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�fB
DB

=B
�B
$�B
/B
9XB
<jB
>wB
G�B
L�B
Q�B
VB
ZB
]/B
bNB
m�B
r�B
t�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111BDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBJBDBDBJBDBPBuB�B�G�O�B!�B&�B/B49B<jBB�BD�BH�BP�B_;Bs�B�VB��B��B�B�B��BǮBǮB��BŢBB�qB�LB�LB�3B�9B�B�B��B��B��B�{B�VB�Bz�Bt�B[#BZBW
BZBB�B?}B>wB'�B�BB��B�`B�B�B��B�B��B��B�uB�By�BjB]/BZB=qB�BB
�mB
�B
�}B
��B
�DB
�DB
n�B
gmB
e`B
^5B
Q�B
;dB
6FB
49B
!�B
�B
 �B	��B	��B	��B	�sB	��B	ƨB	ÖB	�RB	�-B	�B	��B	��B	�+B	r�B	e`B	[#B	XB	P�B	VB	VB	Q�B	M�B	I�B	49B	-B	(�B	�B	DB	DB	B�B�mB�;B		7B	{B	  B��B�B�B�B�)B�B�5B�B��B��BƨBBB�^B�^B�!B�B�B��B��B��B��B��B��B��B��B��B��B��B�oB�uB�hB�VB�JB�PB�JB�%B� B�B� B� B�By�Bs�Bq�Br�Br�Bo�Bn�Bm�BhsBe`BdZBgmBjBl�Bn�Bl�BdZBdZBffBk�Bw�Br�BdZBbNBbNB`BB`BBaHBffB^5BffBiyBl�Bo�Bt�Bt�Bv�Bw�B{�B~�B� B�B�B�B�=B�VB�bB�bB�oB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B��B��B��B��B��B�!B�FB�XB�jBBǮB��B��B��B��B�5B�B��B	B	B	B	1B	JB	PB	\B	\B	bB	hB	hB	hB	hB	hB	oB	oB	uB	�B	�B	�B	�B	uB	{B	�B	�B	�B	%�B	,B	6FB	<jB	C�B	G�B	H�B	J�B	L�B	N�B	P�B	R�B	W
B	YB	^5B	^5B	XB	[#B	^5B	_;B	^5B	]/B	^5B	bNB	l�B	p�B	s�B	y�B	u�B	s�B	x�B	� B	�B	�+B	�1B	�7B	�JB	�VB	�bB	�oB	�hB	�oB	��B	��B	��B	��B	��B	��B	�{B	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�'B	�'B	�3B	�3B	�?B	�?B	�FB	�FB	�FB	�LB	�LB	�RB	�RB	�XB	�XB	�^B	�qB	�}B	��B	��B	B	�}B	�XB	�RB	�LB	�LB	�LB	�RB	�XB	�^B	�jB	�}B	ÖB	��B	�}B	��B	B	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	��B	��B	��B	��B	��B	��B	�B	�)B	�)B	�#B	�B	�B	�
B	�B	�
B	�B	�
B	�B	�B	�/B	�)B	�5B	�;B	�HB	�NB	�HB	�NB	�TB	�NB	�NB	�TB	�TB	�TB	�NB	�TB	�TB	�TB	�`B	�`B	�`B	�ZB	�ZB	�`B	�mB	�yB	�yB	�sB	�fB	�NB	�5B	�/B	�B	�
B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�fB
DB

=B
�B
%�B
0!B
9XB
=qB
>wB
G�B
L�B
Q�B
VB
ZB
]/B
bNB
m�B
r�B
u�11111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<T��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.3 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201101451582012011014515820120110145158  AO  ARGQ                                                                        20111130143642  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130143642  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120110145158  IP                  G�O�G�O�G�O�                