CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-28T23:01:49Z UW 3.1 conversion   
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
_FillValue        G�O�   axis      Z          9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�       C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�       M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       i�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       q�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   y�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       {�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �    SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �0   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �0   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �0   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �0   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �\   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �`   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �d   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �h   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �l   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901072 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               %A   AO  20111130143851  20190522121828  1728_5048_037                   2C  D   APEX                            2142                            040306                          846 @Ԇʚ��1   @Ԇ�@y`@5��/���c9�Q�1   GPS     Primary sampling: mixed [deeper than nominal 990dbar: discrete; nominal 990dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�ff@�  A   A   A@  A`  A�  A�  A�  A���A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C�fC  C   C"  C$  C&  C(  C*  C,�C.�C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv�Cx�Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DF��DGy�DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DMfDM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq�fDrfDr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dy�fD���D�0 D�p D���D�fD�&fD���D��3D���D�3D��3Dǰ D���D��D�vfD�3D��fD�fD�I�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @y��@�ff@�ffA33A;33A[33A{33A���A���A�ffA���A͙�Aݙ�A홚A���B��B��B��B��B&��B.��B6��B>��BF��BN��BV��B^��Bf��Bn��Bv��B~��B�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffC�3C�3C�3C�3C	�3C�3C�3C�3C�3C�3C�3C�3C�3C��C�3C�3C!�3C#�3C%�3C'�3C)�3C+��C-��C/�3C1�3C3�3C5�3C7�3C9�3C;�3C=�3C?�3CA�3CC�3CE�3CG�3CI�3CK�3CM�3CO�3CQ�3CS�3CU�3CW�3CY�3C[�3C]�3C_�3Ca�3Cc�3Ce�3Cg�3Ci�3Ck�3Cm�3Co�3Cq�3Cs�3Cu��Cw��Cy�3C{�3C}�3C�3C�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC��fC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚD l�D ��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��D	l�D	��D
l�D
��Dl�D��Dl�D��Ds3D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��D l�D ��D!l�D!��D"l�D"��D#l�D#��D$l�D$��D%l�D%��D&l�D&��D'l�D'��D(l�D(��D)l�D)��D*l�D*��D+l�D+��D,l�D,��D-l�D-��D.l�D.��D/l�D/��D0l�D0��D1l�D1��D2l�D2��D3l�D3��D4l�D4��D5l�D5��D6l�D6��D7l�D7��D8l�D8��D9l�D9��D:l�D:��D;l�D;��D<l�D<��D=l�D=��D>l�D>��D?l�D?��D@l�D@��DAl�DA��DBl�DB��DCl�DC��DDl�DD��DEl�DE��DFl�DF�fDGffDG��DHl�DH��DIl�DI��DJl�DJ��DKl�DK��DLl�DL�3DMl�DM��DNl�DN��DOl�DO��DPl�DP��DQl�DQ��DRl�DR��DSl�DS��DTl�DT��DUl�DU��DVl�DV��DWl�DW��DXl�DX��DYl�DY��DZl�DZ��D[l�D[��D\l�D\��D]l�D]��D^l�D^��D_l�D_��D`l�D`��Dal�Da��Dbl�Db��Dcl�Dc��Ddl�Dd��Del�De��Dfl�Df��Dgl�Dg��Dhl�Dh��Dil�Di��Djl�Dj��Dkl�Dk��Dll�Dl��Dml�Dm��Dnl�Dn��Dol�Do��Dpl�Dp��Dqs3Dq�3Drl�Dr��Dsl�Ds��Dtl�Dt��Dul�Du��Dvl�Dv��Dwl�Dy�3D�� D�&fD�ffD�� D���D��D��3D���D��3D�	�D�y�DǦfD��3D�3D�l�Dਗ਼D���D���D�@ 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�(�A�$�A�(�A�+A�+A�+A�/A�1'A�{A��;A�`BA�ȴAʇ+A�A�A��yA�A�A�A���A���AȃA�bNA�XAĸRA�XA¾wA�r�A�{A�~�A�ĜA���A��
A��!A��TA�A�ȴA��A�bNA��-A�
=A��!A�ZA�|�A�/A��\A���A���A��PA�A�A��uA�
=A��A��jA�33A�r�A��HA��!A�?}A�ȴA�`BA��A�ƨA���A�5?A�A��A�dZA�1A��hA��mA�x�A�G�A�bA��hA�ĜA�S�A��A��hA�G�A�1A���A��A�t�A�p�A��A�ffA��A���A��A��jA�+A���A��A���A�x�A���A��yA��TA��A�K�A��yA���A�v�A�1'A�5?A�7LA�I�A��hA�\)A�5?A���A��+A���A�~�A�?}A}O�Ay|�Au�As��ArbAoAl�+AkO�Ah�uAfffAb��A`�A^�9A\{A[VAZ�9AZbAX��AW�AWK�AS&�AP5?AN��AL�/AK&�AH�`AGoAEhsACx�ABjAA?}A?�wA=VA;|�A:M�A8M�A6n�A4~�A3l�A2~�A0��A/"�A-�FA,�RA+`BA*�DA)ƨA(�A(ffA'7LA&Q�A%��A%G�A$�+A#�#A#;dA"z�A!�^A ��A {At�AhsA/AVA�A�AI�A��AC�A�HAv�A��A�AA�+Ax�A9XAx�A�DA��A��AQ�A��A"�A�\A9XA�mA;dA�uA�-Al�A7LA�jA�^A
1'A	33A1AhsA��Ax�A/A��A\)A��A��Av�A%A �@��@�5?@�O�@�+@��9@�ȴ@��u@�@�X@�@��@�7L@�I�@�@��@��@睲@�x�@�P@�V@��@���@ޟ�@ݲ-@�1@�V@٩�@��@�A�@��@��#@�1'@Ұ!@љ�@�  @Ώ\@�@���@˕�@��H@��@Ǿw@��@š�@�j@���@�5?@�J@��-@�bN@���@���@��@���@�/@��D@�"�@�E�@�?}@��9@��
@���@��-@���@���@�p�@�G�@���@�C�@�@�p�@��j@� �@�|�@��R@��7@��D@���@��@�M�@��@��@���@�E�@��@�5?@�V@��@�/@��@�@���@���@���@���@�Q�@���@���@��P@�I�@���@�b@��m@�dZ@�o@��y@���@��!@�$�@��7@�p�@�@�{@���@��@��@�7L@��`@��7@��y@�Ĝ@�7L@��@��@�hs@���@��F@���@��@��u@��R@�{@��T@�Q�@�V@��!@�r�@���@���@�(�@��@��;@��m@���@�@�ff@�x�@���@��@�n�@�@��^@��h@�X@�O�@�&�@��`@��u@�Q�@��@�b@��@�S�@�o@�
=@���@���@�ȴ@�@�
=@���@��H@��@���@���@�5?@�hs@���@��@�Q�@� �@�b@��m@�t�@��H@�V@��@�O�@���@�9X@��
@���@��@���@��@��R@�^5@�V@�ȴ@��@��y@��!@��@���@�&�@��@���@���@�A�@��@��;@��;@�ƨ@�|�@�33@�
=@��@�~�@�5?@��@�$�@�J@���@���@�x�@�hs@�`B@���@� �@��@��;@��@�;d@�ȴ@���@��@�Q�@��@���@��y@�l�@�b@��
@���@�~�@��@��@��^@�x�@�hs@��@��@��`@���@�V@�G�@��@���@��@��`@�bN@���@��y@��R@��+@�^5@�E�@��T@��h@�hs@��`@��@;d@x��@q�7@fȴ@]V@T�j@L9X@E�-@>V@7
=@2~�@-�@)�@#ƨ@�@�7@O�@�;@9X@	�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�(�A�$�A�(�A�+A�+A�+A�/A�1'A�{A��;A�`BA�ȴAʇ+A�A�A��yA�A�A�A���A���AȃA�bNA�XAĸRA�XA¾wA�r�A�{A�~�A�ĜA���A��
A��!A��TA�A�ȴA��A�bNA��-A�
=A��!A�ZA�|�A�/A��\A���A���A��PA�A�A��uA�
=A��A��jA�33A�r�A��HA��!A�?}A�ȴA�`BA��A�ƨA���A�5?A�A��A�dZA�1A��hA��mA�x�A�G�A�bA��hA�ĜA�S�A��A��hA�G�A�1A���A��A�t�A�p�A��A�ffA��A���A��A��jA�+A���A��A���A�x�A���A��yA��TA��A�K�A��yA���A�v�A�1'A�5?A�7LA�I�A��hA�\)A�5?A���A��+A���A�~�A�?}A}O�Ay|�Au�As��ArbAoAl�+AkO�Ah�uAfffAb��A`�A^�9A\{A[VAZ�9AZbAX��AW�AWK�AS&�AP5?AN��AL�/AK&�AH�`AGoAEhsACx�ABjAA?}A?�wA=VA;|�A:M�A8M�A6n�A4~�A3l�A2~�A0��A/"�A-�FA,�RA+`BA*�DA)ƨA(�A(ffA'7LA&Q�A%��A%G�A$�+A#�#A#;dA"z�A!�^A ��A {At�AhsA/AVA�A�AI�A��AC�A�HAv�A��A�AA�+Ax�A9XAx�A�DA��A��AQ�A��A"�A�\A9XA�mA;dA�uA�-Al�A7LA�jA�^A
1'A	33A1AhsA��Ax�A/A��A\)A��A��Av�A%A �@��@�5?@�O�@�+@��9@�ȴ@��u@�@�X@�@��@�7L@�I�@�@��@��@睲@�x�@�P@�V@��@���@ޟ�@ݲ-@�1@�V@٩�@��@�A�@��@��#@�1'@Ұ!@љ�@�  @Ώ\@�@���@˕�@��H@��@Ǿw@��@š�@�j@���@�5?@�J@��-@�bN@���@���@��@���@�/@��D@�"�@�E�@�?}@��9@��
@���@��-@���@���@�p�@�G�@���@�C�@�@�p�@��j@� �@�|�@��R@��7@��D@���@��@�M�@��@��@���@�E�@��@�5?@�V@��@�/@��@�@���@���@���@���@�Q�@���@���@��P@�I�@���@�b@��m@�dZ@�o@��y@���@��!@�$�@��7@�p�@�@�{@���@��@��@�7L@��`@��7@��y@�Ĝ@�7L@��@��@�hs@���@��F@���@��@��u@��R@�{@��T@�Q�@�V@��!@�r�@���@���@�(�@��@��;@��m@���@�@�ff@�x�@���@��@�n�@�@��^@��h@�X@�O�@�&�@��`@��u@�Q�@��@�b@��@�S�@�o@�
=@���@���@�ȴ@�@�
=@���@��H@��@���@���@�5?@�hs@���@��@�Q�@� �@�b@��m@�t�@��H@�V@��@�O�@���@�9X@��
@���@��@���@��@��R@�^5@�V@�ȴ@��@��y@��!@��@���@�&�@��@���@���@�A�@��@��;@��;@�ƨ@�|�@�33@�
=@��@�~�@�5?@��@�$�@�J@���@���@�x�@�hs@�`B@���@� �@��@��;@��@�;d@�ȴ@���@��@�Q�@��@���@��y@�l�@�b@��
@���@�~�@��@��@��^@�x�@�hs@��@��@��`@���@�V@�G�@��@���@��@��`@�bN@���@��y@��R@��+@�^5@�E�@��T@��h@�hs@��`@��@;d@x��@q�7@fȴ@]V@T�j@L9X@E�-@>V@7
=@2~�@-�@)�@#ƨ@�@�7@O�@�;@9X@	�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB  B  B  B  B  B  B  B  B  B  BBB��B��B��BB
=B�B�B�B{B{B)�B@�BXB\)BXBL�B?}BH�Bw�B�7B�hB��B�'B��BBɺB��B�5B�HB�
B��B��BǮBB�?B��B��B��B�!B�wB��B�FB�B�3B�'B�RB��B�/B�NB�mB�TB�sB�TB�;B�B��B��B��B��B��B��B��B�}B�qB�RB�FB�3B�B��B�{B�1By�Br�Bm�BjB\)BP�BI�BJ�BH�BB�B7LB.B�BB�TBƨB�^B��B�DBYB�B
�B
ǮB
��B
�%B
o�B
T�B
B�B
%�B
\B	��B	�NB	��B	��B	��B	��B	�oB	n�B	dZB	R�B	:^B	�B	1B	B��B	�B	8RB	?}B	9XB	1'B	&�B	B�B�B�B�5BɺB�XB�RB��B��B�\B�JB�By�Bu�Bm�BjBhsBffBcTBe`BhsB`BB_;BaHB]/B\)B[#B\)B^5B`BBdZB^5BffBhsBffB\)B`BBe`BbNB_;BbNBjBgmBdZBgmBaHB[#B\)B\)B]/B\)B[#B[#B\)B\)B\)BYBXBW
BYBW
BW
BYBYBW
BVBYB\)B]/BcTBbNBaHB_;Be`B\)BT�BO�BJ�BH�BG�BD�BA�BA�BA�BF�B?}B<jB:^B8RB7LB6FB=qB1'B6FB6FB1'B5?B7LB2-B/B33B1'B.B1'B1'B-B,B)�B+B)�B+B-B+B)�B(�B+B)�B(�B+B+B+B,B-B-B.B/B/B2-B2-B2-B33B6FB6FB9XB>wB@�B?}B>wB?}BA�BF�BK�BO�BP�BN�BN�BN�BN�BM�BVB[#BaHBiyBm�Bn�BhsBk�Bu�BjBm�Bs�Br�Bt�By�Bq�Br�Bx�Bv�Bw�Bw�B|�B�1B�=B�VB�{B��B��B�{B��B��B��B��B��B��B��B�-B�RB�wBÖB��B�B�B�#B�BB�NB�mB�B�B��B��B��B	B	B	B	B	B	%B	�B	(�B	1'B	(�B	&�B	)�B	�B	!�B	$�B	#�B	>wB	P�B	W
B	ZB	N�B	<jB	.B	1'B	;dB	L�B	P�B	hsB	k�B	k�B	jB	jB	jB	hsB	ffB	bNB	bNB	dZB	ffB	jB	m�B	m�B	m�B	l�B	l�B	m�B	u�B	{�B	}�B	�B	�B	�B	�B	�%B	�7B	�7B	�7B	�=B	�DB	�DB	�DB	�JB	�VB	�bB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�-B	�9B	�FB	�9B	�3B	�LB	�LB	�RB	�XB	�^B	�jB	�jB	�}B	��B	��B	B	ÖB	ÖB	ĜB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ɺB	ȴB	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�)B	�BB	�ZB	�`B	�`B	�mB	�B	�B	�B	�sB	�`B	�`B	�ZB	�TB	�ZB	�ZB	�sB	�mB	�`B	�fB	��B
B
hB
�B
�B
.B
5?B
>wB
F�B
J�B
O�B
S�B
YB
]/B
e`B
jB
p�B
u�B
w�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B  B  B  B  B  B  B  BBBBBB  B  BBB
=B�B�B�B�B�B/BB�BYB^5BZBO�B?}BH�Bx�B�JB��B��B�9BȴBƨB��B��B�BB�fB�B��B��B��BɺB�dB��B��B��B�-B��BÖB�XB�!B�?B�3B�^B��B�;B�mB�B�ZB�B�`B�HB�#B�B��B��B��B��B��BÖBB��B�^B�RB�FB�?B��B��B�DB}�Bu�Bp�Br�BaHBT�BM�BN�BJ�BD�B:^B33B�B	7B�yBȴB��B��B��BcTB#�B
��B
��B
�'B
�PB
w�B
]/B
H�B
-B
�B
%B	�B	��B	�B	��B	��B	��B	s�B	m�B	ZB	C�B	 �B	DB	
=B	  B	�B	:^B	B�B	<jB	33B	1'B	PB�B��B�B�`B��B�wB�qB��B��B�{B�{B�%B}�B{�Bs�Bp�Bl�BiyBiyBk�Bm�BdZBdZBdZB`BB_;B]/BaHBaHBbNBffB`BBhsBjBhsB^5BcTBgmBdZB_;BcTBm�BiyBffBjBcTB\)B^5B^5B_;B^5B]/B]/B_;B`BB_;B\)B[#BYB[#BZBYB[#B[#BYBYB\)B_;B^5BdZBdZBe`Be`BiyB`BBXBQ�BO�BJ�BJ�BI�BC�BB�BC�BL�BC�B?}B<jB:^B;dB;dBA�B5?B8RB8RB49B7LB8RB49B1'B5?B2-B0!B49B49B/B.B-B,B,B.B0!B,B+B+B-B,B,B.B-B.B/B.B/B0!B1'B33B49B49B5?B6FB9XB7LB:^B?}BC�BB�B?}B@�BB�BH�BM�BR�BR�BP�BO�BN�BR�BN�BVB[#BbNBjBn�Bp�BiyBm�Bv�Bk�Bn�Bt�Bt�Bv�Bz�Br�Bs�By�Bv�Bw�Bw�B{�B�7B�=B�\B��B��B��B�{B��B��B��B��B��B��B��B�'B�RB�}BĜB��B�
B�#B�)B�BB�TB�sB�B�B��B��B��B	B	+B	B	B	B	B	�B	)�B	2-B	-B	(�B	+B	�B	!�B	#�B	 �B	9XB	Q�B	ZB	^5B	T�B	<jB	.B	/B	8RB	K�B	L�B	hsB	k�B	m�B	k�B	l�B	k�B	jB	iyB	cTB	bNB	dZB	gmB	jB	m�B	n�B	n�B	m�B	m�B	m�B	v�B	|�B	~�B	�B	�B	�B	�B	�%B	�7B	�7B	�7B	�=B	�DB	�JB	�DB	�JB	�\B	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�-B	�?B	�LB	�?B	�9B	�RB	�LB	�XB	�^B	�dB	�jB	�jB	�}B	��B	B	B	ĜB	ĜB	ŢB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ȴB	��B	��B	��B	��B	�B	��B	�B	��B	��B	��B	��B	�
B	�
B	�B	�B	�)B	�BB	�`B	�`B	�`B	�sB	�B	�B	�B	�sB	�`B	�`B	�ZB	�ZB	�`B	�ZB	�yB	�sB	�`B	�mB	��B
B
hB
�B
�B
/B
5?B
>wB
F�B
J�B
O�B
S�B
YB
]/B
e`B
k�B
p�B
u�B
w�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<49X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<D��<T��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.3 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201101452052012011014520520120110145206  AO  ARGQ                                                                        20111130143851  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130143851  QCF$                G�O�G�O�G�O�4000            UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120110145206  IP                  G�O�G�O�G�O�                