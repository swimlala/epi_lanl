CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-28T23:01:48Z UW 3.1 conversion   
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
_FillValue        G�O�   axis      Z          9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�       C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�       M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       i�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       q�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   z   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       |   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �$   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �T   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �T   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �T   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �T   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    � Argo profile    3.1 1.2 19500101000000  5901072 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL                A   AO  20111130143819  20190522121828  1728_5048_032                   2C  D   APEX                            2142                            040306                          846 @�zcA��1   @�zd��@@4�\(��cj��+1   GPS     Primary sampling: mixed [deeper than nominal 990dbar: discrete; nominal 990dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�ff@�  A��A   A@  A`  A�  A�  A���A���A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  BpffBw��B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\�C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D�fD  D� D  D�fD   D y�D!  D!� D"  D"y�D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm�fDn  Dn�fDo  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dws3Dy��D�fD�P D���D���D��fD�6fD�y�D�� D�fD�  D�� Dǩ�D��3D�  Dڌ�D� D��fD�3D�S3D�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @y��@�ff@���A33A;33A[33A{33A���A�ffA�ffA���A͙�Aݙ�A홚A���B��B��B��B��B&��B.��B6��B>��BF��BN��BV��B^��Bf��Bo33BvffB~��B�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffC�3C�3C�3C�3C	�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C!�3C#�3C%�3C'�3C)�3C+�3C-�3C/�3C1�3C3�3C5�3C7�3C9�3C;�3C=�3C?�3CA�3CC�3CE�3CG�3CI�3CK�3CM�3CO�3CQ�3CS�3CU�3CW�3CY�3C[��C]�3C_�3Ca�3Cc�3Ce�3Cg�3Ci�3Ck�3Cm�3Co�3Cq�3Cs�3Cu�3Cw�3Cy�3C{�3C}�3C�3C�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC���C�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC���C�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚD l�D ��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��D	l�D	��D
l�D
��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D�fDl�D��Dl�D��Dl�D��Ds3D��Dl�D��Ds3D��D ffD ��D!l�D!��D"ffD"��D#l�D#��D$l�D$��D%l�D%��D&l�D&��D'l�D'��D(l�D(��D)l�D)��D*l�D*��D+l�D+��D,l�D,��D-l�D-��D.l�D.��D/l�D/��D0l�D0��D1l�D1��D2l�D2��D3l�D3��D4l�D4��D5l�D5��D6l�D6��D7l�D7��D8l�D8��D9l�D9��D:l�D:��D;l�D;��D<l�D<��D=l�D=��D>l�D>��D?l�D?��D@l�D@��DAl�DA��DBl�DB��DCl�DC��DDl�DD��DEl�DE��DFl�DF��DGl�DG��DHl�DH��DIl�DI��DJl�DJ��DKl�DK��DLl�DL��DMl�DM��DNl�DN��DOl�DO��DPl�DP��DQl�DQ��DRl�DR��DSl�DS��DTl�DT��DUl�DU��DVl�DV��DWl�DW��DXl�DX��DYl�DY��DZl�DZ��D[l�D[��D\l�D\��D]l�D]��D^l�D^��D_l�D_��D`l�D`��Dal�Da��Dbl�Db��Dcl�Dc��Ddl�Dd��Del�De��Dfl�Df��Dgl�Dg��Dhl�Dh��Dil�Di��Djl�Dj��Dkl�Dk��Dll�Dl��Dms3Dm��Dns3Dn��Dol�Do��Dpl�Dp��Dql�Dq��Drl�Dr��Dsl�Ds��Dtl�Dt��Dul�Du��Dvl�Dv��Dw` Dy��D���D�FfD�� D��3D���D�,�D�p D��fD���D�fD��fDǠ D�ٚD�fDڃ3D�fD���D�	�D�I�D��f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�bA�VA�JA�
=A�  A�A��
A�z�A��AǇ+A�1'A�A�AƸRA�ĜA�A�A���AƓuA�z�A�p�A�\)A�bA���Ař�A�~�A�XA�
=AăA�
=A�l�A�$�A�\)A���A���A��#A�M�A�%A���A�XA��/A�|�A�1'A� �A�oA��jA��A�E�A�33A��A��A�bNA��#A�O�A��mA�G�A���A��PA��9A�5?A�S�A�I�A�v�A�Q�A�5?A�oA���A�JA���A�;dA�x�A���A�VA��DA��#A���A�n�A���A�&�A��+A��mA�t�A��A�9XA��A�A���A�5?A�?}A���A��A�?}A�ƨA���A�E�A�x�A��;A�G�A��HA��A��A�1A�A�hsA���A�XA;dAyoAw\)Au�#Aq�
AnjAm�Al�AlȴAl��Ak�
Ah��Ae�Ad��AbĜA`r�A_O�A]x�A[?}AX1AVn�AU+AR-AP�AO7LAM
=AI��AG�-AEp�AD�RADM�ACC�ABZA@ĜA?�wA>�A=��A<��A;��A:1A8 �A7��A733A6�+A5�A5G�A4~�A4(�A3/A21'A1XA.��A-A,��A,I�A+��A+�-A+33A*�A*M�A)��A)S�A(�DA& �A%��A%"�A$�jA$�A#p�A"��A!�A�^A�HA�Al�AVA�`A�jA�AoA�AO�AE�A�FA��A�9A~�A�AS�A��A�;A�DA�FA��A��A"�A
�A
$�Av�A�
A��AAbNA9XA(�A�AbA��A1'A �A{A��A�#A��AXA �`@�"�@��`@��
@�o@��@�+@�@�1'@�w@��@�h@�ȴ@���@�n�@�v�@�v�@�7@�  @�+@�
=@�&�@�j@�w@�V@�/@���@�`B@�(�@���@�A�@��#@��`@�r�@� �@ӍP@���@Χ�@���@̋D@�K�@�`B@ȃ@�t�@��@�v�@��`@���@��D@�n�@��H@���@�b@�t�@�ff@�hs@���@�@�{@���@��@��@��@�G�@��@�(�@��m@�ƨ@�\)@�"�@��@�@�o@�o@��@��H@��@��!@�5?@��@���@�z�@�A�@�"�@�z�@�A�@��j@��@�$�@�  @�5?@���@��@��/@��@��\@�b@�ff@���@�1@���@�n�@�V@�=q@�5?@�=q@�E�@��@���@��@���@�/@�r�@��@��u@�j@��m@��P@�;d@�
=@��@��!@�E�@��@���@��-@�@��^@��@��@��@�x�@���@�+@���@���@�(�@��m@��F@��F@��@��@��@��H@��+@�{@�O�@�Q�@�ƨ@�
=@�~�@���@�/@�/@���@��;@��\@��@�A�@�ƨ@�S�@���@�^5@��T@���@��h@�x�@�p�@���@�r�@�9X@��@���@�  @�1@�b@��F@�C�@���@��+@�M�@��T@�%@�Q�@��m@��@�l�@�C�@�@���@�^5@�{@��T@���@���@�x�@�?}@�G�@�`B@�hs@�x�@�hs@�/@���@�bN@�  @��m@��w@���@��
@� �@���@���@��j@���@��F@�"�@��!@�{@�$�@�$�@�5?@���@��^@�X@�`B@��h@�n�@�ƨ@���@�|�@�l�@�\)@�+@�
=@���@��+@�ff@�E�@�@���@���@��h@�`B@�%@�z�@�I�@�1@���@�l�@�K�@�+@�33@�S�@�;d@�o@��y@��!@��\@�v�@�v�@�n�@�ff@�^5@�=q@�$�@��-@�G�@�%@���@��u@�j@�Z@�@x  @n��@dz�@_�P@Z�\@S"�@KdZ@Ct�@;o@1�@)�#@$�@ �`@�m@�@&�@��@	�^@;d@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�bA�VA�JA�
=A�  A�A��
A�z�A��AǇ+A�1'A�A�AƸRA�ĜA�A�A���AƓuA�z�A�p�A�\)A�bA���Ař�A�~�A�XA�
=AăA�
=A�l�A�$�A�\)A���A���A��#A�M�A�%A���A�XA��/A�|�A�1'A� �A�oA��jA��A�E�A�33A��A��A�bNA��#A�O�A��mA�G�A���A��PA��9A�5?A�S�A�I�A�v�A�Q�A�5?A�oA���A�JA���A�;dA�x�A���A�VA��DA��#A���A�n�A���A�&�A��+A��mA�t�A��A�9XA��A�A���A�5?A�?}A���A��A�?}A�ƨA���A�E�A�x�A��;A�G�A��HA��A��A�1A�A�hsA���A�XA;dAyoAw\)Au�#Aq�
AnjAm�Al�AlȴAl��Ak�
Ah��Ae�Ad��AbĜA`r�A_O�A]x�A[?}AX1AVn�AU+AR-AP�AO7LAM
=AI��AG�-AEp�AD�RADM�ACC�ABZA@ĜA?�wA>�A=��A<��A;��A:1A8 �A7��A733A6�+A5�A5G�A4~�A4(�A3/A21'A1XA.��A-A,��A,I�A+��A+�-A+33A*�A*M�A)��A)S�A(�DA& �A%��A%"�A$�jA$�A#p�A"��A!�A�^A�HA�Al�AVA�`A�jA�AoA�AO�AE�A�FA��A�9A~�A�AS�A��A�;A�DA�FA��A��A"�A
�A
$�Av�A�
A��AAbNA9XA(�A�AbA��A1'A �A{A��A�#A��AXA �`@�"�@��`@��
@�o@��@�+@�@�1'@�w@��@�h@�ȴ@���@�n�@�v�@�v�@�7@�  @�+@�
=@�&�@�j@�w@�V@�/@���@�`B@�(�@���@�A�@��#@��`@�r�@� �@ӍP@���@Χ�@���@̋D@�K�@�`B@ȃ@�t�@��@�v�@��`@���@��D@�n�@��H@���@�b@�t�@�ff@�hs@���@�@�{@���@��@��@��@�G�@��@�(�@��m@�ƨ@�\)@�"�@��@�@�o@�o@��@��H@��@��!@�5?@��@���@�z�@�A�@�"�@�z�@�A�@��j@��@�$�@�  @�5?@���@��@��/@��@��\@�b@�ff@���@�1@���@�n�@�V@�=q@�5?@�=q@�E�@��@���@��@���@�/@�r�@��@��u@�j@��m@��P@�;d@�
=@��@��!@�E�@��@���@��-@�@��^@��@��@��@�x�@���@�+@���@���@�(�@��m@��F@��F@��@��@��@��H@��+@�{@�O�@�Q�@�ƨ@�
=@�~�@���@�/@�/@���@��;@��\@��@�A�@�ƨ@�S�@���@�^5@��T@���@��h@�x�@�p�@���@�r�@�9X@��@���@�  @�1@�b@��F@�C�@���@��+@�M�@��T@�%@�Q�@��m@��@�l�@�C�@�@���@�^5@�{@��T@���@���@�x�@�?}@�G�@�`B@�hs@�x�@�hs@�/@���@�bN@�  @��m@��w@���@��
@� �@���@���@��j@���@��F@�"�@��!@�{@�$�@�$�@�5?@���@��^@�X@�`B@��h@�n�@�ƨ@���@�|�@�l�@�\)@�+@�
=@���@��+@�ff@�E�@�@���@���@��h@�`B@�%@�z�@�I�@�1@���@�l�@�K�@�+@�33@�S�@�;d@�o@��y@��!@��\@�v�@�v�@�n�@�ff@�^5@�=q@�$�@��-@�G�@�%@���@��u@�j@�Z@�@x  @n��@dz�@_�P@Z�\@S"�@KdZ@Ct�@;o@1�@)�#@$�@ �`@�m@�@&�@��@	�^@;d@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�{B�{B�{B�{B�{B�uB�{B��B��B��B�B�!B�3B�9B�3B�3B�3B�3B�?B�?B�?B�?B�?B�FB�XB�^B�jB�}B�wB�XBĜB�BbB9XBK�BF�BC�BJ�BffB}�B�{B��B�B�B��B��B��B�B��B��B��B��B��B��B�\B�\B�VB�VB�{B�bB�By�Br�Bq�Br�Bs�Br�By�Bx�Bk�BiyBjBdZBcTB^5BE�B(�B�BDBB��B��B�B�sB�`B�5B�
B��B�RB��B��B�DBp�B^5BL�B33B"�BbB
�B
��B
�?B
�1B
u�B
q�B
\)B
?}B
(�B
PB	��B	�5B	ĜB	��B	��B	��B	��B	��B	��B	��B	m�B	ffB	k�B	ffB	jB	hsB	D�B	1'B	2-B	$�B	�B	{B	uB	DB	B��B�B�B�sB�`B�HB�TB�BB�#B�
B��B��B��BɺBǮBǮB��BǮB��B��BƨB�wBƨB�^B��B�?B�9B�RB�LB�3B�-B�9B�-B�B�'B�B�9B�9B�B�B�B�B�B��B�'B�B�B��B��B��B��B��B��B��B��B��B��B�oB��B��B�{B�\B�DB�=B�7B�1B�+B�B�B�B~�B�B�VB��B��B��B�bB��B��B��B�uB��B��B��B��B��B��B��B��B��B��B��B��B�Bn�BgmBl�Bl�Bt�Bz�B�VB��B��B��B��B��B��B��B��B��B��B��B��B�{B�oB�oB�bB�oB�uB�VB�JB�JB�JB�JB�7B�%B�B�+B�7B�%B�B�B�B�DB�7B�+B�B� B� B}�B}�Bx�Bs�Bt�Br�Bq�B�PB�7B�DB�DB�+B�DB�\B�\B�VB�\B�bB�oB�{B��B��B��B��B�B�B�B�B�-B�B�B�!B�B��B�'B�dB�dB�?B�9B�?B�FB�LB�LB�XB��B��B��BɺB��B��B��B��B�B�B�5B�sB�B�B�B�B�B�B��B	B	DB	bB	bB	hB	oB	uB	{B	�B	�B	'�B	.B	2-B	33B	6FB	:^B	=qB	E�B	O�B	W
B	[#B	cTB	k�B	m�B	n�B	o�B	o�B	q�B	r�B	q�B	q�B	r�B	p�B	o�B	p�B	q�B	p�B	o�B	n�B	o�B	o�B	n�B	m�B	l�B	m�B	n�B	o�B	r�B	s�B	v�B	w�B	x�B	y�B	|�B	�B	�B	�1B	�DB	�JB	�PB	�VB	�\B	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�'B	�'B	�?B	�XB	�qB	�}B	��B	ÖB	ÖB	ÖB	ŢB	B	B	B	B	ĜB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�
B	�)B	�ZB	�fB	�mB	�B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B	��B	��B	��B	��B	��B
B
VB
�B
"�B
(�B
-B
49B
;dB
B�B
K�B
O�B
ZB
YB
^5B
dZB
hsB
o�B
v�B
y�B
~�B
~�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�{B�{B�{B�{B�{B�{B��B��B��B�B�B�'B�3B�9B�3B�3B�3B�9B�FB�?B�?B�FB�FB�LB�^B�dB�qB��B�}B�jBȴB�B�B=qBO�BH�BD�BK�BhsB� B��B��B�B�B�B�B�B�'B��B��B��B��B��B��B�oB�bB�bB�oB��B�{B�PB}�Bs�Br�Bs�Bt�Bz�B�B� Bo�Bm�Bn�BgmBgmBiyBR�B1'B�B\B1B��B��B�B�yB�fB�;B�B��B�}B�B��B�oBv�Be`BVB;dB&�B�B
�B
�B
�wB
�VB
y�B
z�B
e`B
P�B
:^B
oB	��B	�fB	��B	��B	��B	��B	��B	��B	��B	��B	p�B	jB	p�B	iyB	o�B	n�B	L�B	6FB	6FB	-B	 �B	�B	�B	�B	1B��B�B�B�B�sB�fB�fB�TB�;B�#B�B�B��B��BɺBɺB��BɺB��B��BɺB��BȴB��BŢB�FB�?B�XB�RB�?B�9B�?B�9B�!B�9B�9B�FB�FB�!B�'B�B�B�-B�!B�9B�B�B��B��B��B��B�B��B��B��B��B��B�uB��B��B��B�oB�VB�VB�JB�JB�=B�%B�%B�B�B�+B�\B��B��B��B�bB��B��B��B�{B��B��B��B��B��B��B��B��B��B��B��B��B�3Br�BhsBm�Bk�Bs�Bx�B�VB��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B��B��B��B�bB�PB�PB�PB�\B�VB�7B�%B�=B�JB�1B�B�B�%B�VB�DB�PB�B� B�B�B~�Bz�Bu�Bu�Bq�Bq�B�VB�DB�PB�JB�1B�JB�bB�bB�VB�bB�hB�uB�{B��B��B��B��B�B�B�B�!B�3B�!B�B�-B�-B��B�'B�qB�wB�XB�LB�FB�LB�LB�LB�LB�}B��B��BɺB��B��B��B��B�B�B�5B�yB�B�B�B�B�B�B��B	B	JB	hB	hB	oB	oB	{B	�B	�B	�B	'�B	.B	2-B	49B	7LB	:^B	=qB	D�B	O�B	W
B	[#B	cTB	l�B	n�B	n�B	o�B	o�B	r�B	s�B	r�B	r�B	s�B	r�B	p�B	q�B	r�B	r�B	p�B	n�B	p�B	q�B	p�B	o�B	n�B	n�B	o�B	p�B	s�B	t�B	w�B	w�B	x�B	y�B	}�B	�B	�%B	�1B	�DB	�JB	�PB	�VB	�bB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�'B	�-B	�?B	�XB	�qB	�}B	��B	ĜB	ĜB	ĜB	ƨB	B	B	B	B	ĜB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	��B	��B	�B	�B	�ZB	�mB	�mB	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B	��B
  B	��B	��B	��B
B
VB
�B
"�B
(�B
-B
49B
;dB
B�B
K�B
P�B
ZB
YB
^5B
dZB
hsB
o�B
v�B
y�B
~�B
~�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<T��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<D��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<�o<�C�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.3 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201101452042012011014520420120110145204  AO  ARGQ                                                                        20111130143819  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130143819  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120110145204  IP                  G�O�G�O�G�O�                