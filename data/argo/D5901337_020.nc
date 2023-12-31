CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-12-05 AOML 2.2 creation; 2015-12-13T23:15:29Z UW 3.1 conversion   
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
_FillValue                    � Argo profile    3.1 1.2 19500101000000  5901337 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               A   AO  20111205112644  20190522121836  1901_5055_020                   2C  D   APEX                            2140                            040306                          846 @�[���1   @�[d���@0ݲ-V�cRfffff1   GPS     Primary sampling: mixed [deeper than nominal 990dbar: discrete; nominal 990dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BW��B`  Bh  Bp  Bx  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33C   C  C  C  C  C
  C  C  C  C  C�C�C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?fD?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DWfDW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\�fD]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dy��D�3D�FfD��3D���D���D�#3D�ffD���D���D�)�D�VfDǜ�D�	�D�33D�|�Dਗ਼D��fD�#3D�i�D��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�33@�ff@�ffA33A;33A[33A{33A���A���A���A���A͙�Aݙ�A홚A���B��B��B��B��B&��B.��B6��B>��BF��BN��BVffB^��Bf��Bn��Bv��B~��B�ffB���B�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB���BÙ�B�ffB˙�B�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB���B�ffC�3C�3C�3C�3C	�3C�3C�3C�3C�3C��C��C�3C�3C�3C�3C�3C!�3C#�3C%�3C'�3C)�3C+�3C-�3C/�3C1�3C3�3C5�3C7�3C9�3C;�3C=�3C?�3CA�3CC�3CE�3CG�3CI�3CK�3CM�3CO�3CQ�3CS�3CU�3CW�3CY�3C[�3C]�3C_�3Ca�3Cc�3Ce�3Cg�3Ci�3Ck�3Cm�3Co�3Cq�3Cs�3Cu�3Cw�3Cy�3C{�3C}�3C�3C�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC��fC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚD l�D ��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��D	l�D	��D
l�D
��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D�fDl�D��Dl�D�3Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��D l�D ��D!l�D!��D"l�D"��D#l�D#��D$l�D$��D%l�D%��D&l�D&��D'l�D'��D(l�D(��D)l�D)��D*l�D*��D+l�D+��D,l�D,��D-l�D-��D.l�D.��D/l�D/��D0l�D0��D1l�D1��D2l�D2��D3l�D3��D4l�D4��D5l�D5��D6l�D6��D7l�D7��D8l�D8��D9l�D9��D:l�D:��D;l�D;��D<l�D<��D=l�D=��D>l�D>�3D?l�D?��D@l�D@��DAl�DA��DBl�DB��DCl�DC��DDl�DD��DEl�DE��DFl�DF��DGl�DG��DHl�DH��DIl�DI��DJl�DJ��DKl�DK��DLl�DL��DMl�DM��DNl�DN��DOl�DO��DPl�DP��DQl�DQ��DRl�DR��DSl�DS��DTl�DT��DUl�DU��DVl�DV�3DWl�DW��DXl�DX��DYl�DY��DZl�DZ��D[l�D[��D\s3D\��D]l�D]��D^l�D^��D_l�D_��D`l�D`��Dal�Da��Dbl�Db��Dcl�Dc��Ddl�Dd��Del�De��Dfl�Df��Dgl�Dg��Dhl�Dh��Dil�Di��Djl�Dj��Dkl�Dk��Dll�Dl��Dml�Dm��Dnl�Dn��Dol�Do��Dpl�Dp��Dql�Dq��Drl�Dr��Dsl�Ds��Dtl�Dt��Dul�Du��Dvl�Dv��Dwl�Dy��D�	�D�<�D�y�D��3D��3D��D�\�D��3D��3D�  D�L�DǓ3D�  D�)�D�s3D� D���D��D�` D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��TA��TA��`A��yA��A��A��A��A��A��A���A���A��mA��`A��mA��mA��mA��mA��A��mA��/A��/A��
A���A�ĜA�A���AǼjAǺ^AǸRAǶFAǲ-Aǰ!AǬAǧ�Aǥ�Aǣ�Aǣ�Aǡ�Aǟ�AǙ�AǏ\AǅA�v�A�n�A�p�A�C�A��#A�z�A�VA�oA�\)A��A���A�VA���A�v�A��A��A�(�A�bNA� �A��7A��RA�p�A��FA���A��yA��RA�$�A��A��A�bNA���A�XA�VA��A�~�A���A�9XA�?}A~��A}�A{K�Ay�Aw�AsAqx�Ao�AoAm
=Ai�Ac�A^�!A]�A\Q�AZbAWO�AV�uAU��AO�AKp�AE�A@�/A=�#A;�^A8I�A4A�A3`BA2�A2�A1%A0z�A/K�A-O�A,$�A+��A*��A)��A(�A(�A';dA%�7A$1A"ZA!�;A!�7A!�A �/A jAt�A��A9XA�hA�`A�-A+AbA�TA�+A�PA�yA"�Az�A�jAQ�A?}AdZA�AXAt�A(�A��A�A��A1'A��Av�A��A  A��A1A�
Ax�A��AA�A%A
=AĜA��A��A�AjA��A/A�`AjA�A�A�A=qA+A��A��A�AZAA�TA��A
�DA
$�A	�A	��A	\)A	A	+A	oA��At�A�/A~�A�AC�AA�A{A�wA��A�RA�/A�9A;dAȴA{A  A�-A ��A VA 9X@�hs@���@��9@�1@���@�=q@��7@��@��j@�Z@�9X@���@���@��`@�Ĝ@��@�w@�\)@�R@��#@�9@�@�C�@�R@�M�@���@��@�G�@�j@�@�Q�@���@�@�l�@�C�@ꟾ@�-@��@�r�@�(�@��
@�S�@�~�@��@�O�@��/@��@�@���@�v�@�C�@ᙚ@�Z@�A�@�I�@��y@��
@��@�{@��#@�hs@��D@���@��@�t�@�{@ݺ^@�@݁@�`B@�V@ܴ9@��m@�@�^5@��T@�-@�ff@ڇ+@��@���@�X@��`@�I�@׶F@�S�@�;d@��y@�$�@��@���@�`B@Դ9@�j@�1'@ӥ�@�S�@�"�@ҧ�@�@�G�@���@�j@��@υ@�"�@Χ�@�^5@�J@́@�Ĝ@̬@̛�@�j@�1'@˾w@��@�v�@�^5@�=q@���@���@ɉ7@�7L@�Q�@� �@�  @��;@�ƨ@ǶF@Ǯ@�dZ@�ȴ@Ɨ�@�M�@��@š�@�hs@��`@�9X@�K�@���@�@�@\@\@�v�@�=q@��@���@�?}@�%@��@�1@��F@�33@�@��@�ff@�@�/@��`@��j@��u@�Q�@� �@��m@��@�"�@���@��\@�=q@��@�{@��@��7@�G�@�Ĝ@�z�@�b@��P@���@�@��-@�hs@��`@�z�@�9X@��m@��
@�dZ@��y@�ȴ@��+@�M�@���@���@��^@���@�A�@�b@��;@��@�S�@��!@���@�n�@��@�X@���@�bN@�(�@�  @��P@��@�ȴ@���@���@�E�@���@�?}@��@���@�z�@�1'@��;@�o@�~�@��#@�hs@���@��u@�9X@�dZ@��@���@��#@���@� �@�ƨ@�dZ@���@��@��T@��h@�X@��@��D@��@��@��P@�dZ@�+@�@���@�n�@�-@�$�@�n�@��!@��+@��@���@�x�@�`B@��@���@��D@��@�j@�9X@��@�b@��;@���@�ȴ@���@��@��@��@|��@qx�@i�^@^5?@V��@O
=@HA�@?l�@5/@.ȴ@)�#@%/@ 1'@�
@�-@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��TA��TA��`A��yA��A��A��A��A��A��A���A���A��mA��`A��mA��mA��mA��mA��A��mA��/A��/A��
A���A�ĜA�A���AǼjAǺ^AǸRAǶFAǲ-Aǰ!AǬAǧ�Aǥ�Aǣ�Aǣ�Aǡ�Aǟ�AǙ�AǏ\AǅA�v�A�n�A�p�A�C�A��#A�z�A�VA�oA�\)A��A���A�VA���A�v�A��A��A�(�A�bNA� �A��7A��RA�p�A��FA���A��yA��RA�$�A��A��A�bNA���A�XA�VA��A�~�A���A�9XA�?}A~��A}�A{K�Ay�Aw�AsAqx�Ao�AoAm
=Ai�Ac�A^�!A]�A\Q�AZbAWO�AV�uAU��AO�AKp�AE�A@�/A=�#A;�^A8I�A4A�A3`BA2�A2�A1%A0z�A/K�A-O�A,$�A+��A*��A)��A(�A(�A';dA%�7A$1A"ZA!�;A!�7A!�A �/A jAt�A��A9XA�hA�`A�-A+AbA�TA�+A�PA�yA"�Az�A�jAQ�A?}AdZA�AXAt�A(�A��A�A��A1'A��Av�A��A  A��A1A�
Ax�A��AA�A%A
=AĜA��A��A�AjA��A/A�`AjA�A�A�A=qA+A��A��A�AZAA�TA��A
�DA
$�A	�A	��A	\)A	A	+A	oA��At�A�/A~�A�AC�AA�A{A�wA��A�RA�/A�9A;dAȴA{A  A�-A ��A VA 9X@�hs@���@��9@�1@���@�=q@��7@��@��j@�Z@�9X@���@���@��`@�Ĝ@��@�w@�\)@�R@��#@�9@�@�C�@�R@�M�@���@��@�G�@�j@�@�Q�@���@�@�l�@�C�@ꟾ@�-@��@�r�@�(�@��
@�S�@�~�@��@�O�@��/@��@�@���@�v�@�C�@ᙚ@�Z@�A�@�I�@��y@��
@��@�{@��#@�hs@��D@���@��@�t�@�{@ݺ^@�@݁@�`B@�V@ܴ9@��m@�@�^5@��T@�-@�ff@ڇ+@��@���@�X@��`@�I�@׶F@�S�@�;d@��y@�$�@��@���@�`B@Դ9@�j@�1'@ӥ�@�S�@�"�@ҧ�@�@�G�@���@�j@��@υ@�"�@Χ�@�^5@�J@́@�Ĝ@̬@̛�@�j@�1'@˾w@��@�v�@�^5@�=q@���@���@ɉ7@�7L@�Q�@� �@�  @��;@�ƨ@ǶF@Ǯ@�dZ@�ȴ@Ɨ�@�M�@��@š�@�hs@��`@�9X@�K�@���@�@�@\@\@�v�@�=q@��@���@�?}@�%@��@�1@��F@�33@�@��@�ff@�@�/@��`@��j@��u@�Q�@� �@��m@��@�"�@���@��\@�=q@��@�{@��@��7@�G�@�Ĝ@�z�@�b@��P@���@�@��-@�hs@��`@�z�@�9X@��m@��
@�dZ@��y@�ȴ@��+@�M�@���@���@��^@���@�A�@�b@��;@��@�S�@��!@���@�n�@��@�X@���@�bN@�(�@�  @��P@��@�ȴ@���@���@�E�@���@�?}@��@���@�z�@�1'@��;@�o@�~�@��#@�hs@���@��u@�9X@�dZ@��@���@��#@���@� �@�ƨ@�dZ@���@��@��T@��h@�X@��@��D@��@��@��P@�dZ@�+@�@���@�n�@�-@�$�@�n�@��!@��+@��@���@�x�@�`B@��@���@��D@��@�j@�9X@��@�b@��;@���@�ȴ@���@��@��@��@|��@qx�@i�^@^5?@V��@O
=@HA�@?l�@5/@.ȴ@)�#@%/@ 1'@�
@�-@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	m�B	m�B	m�B	m�B	m�B	m�B	m�B	m�B	m�B	m�B	m�B	m�B	n�B	n�B	n�B	n�B	n�B	n�B	n�B	n�B	n�B	n�B	o�B	o�B	p�B	p�B	q�B	q�B	r�B	s�B	s�B	t�B	u�B	u�B	v�B	w�B	x�B	y�B	y�B	z�B	{�B	{�B	}�B	� B	�B	�B	�%B	�\B
DB
v�B
�7B
}�B
o�B
� B
�B
|�B
w�B
iyB
�JB
�3B
�B�Bl�B�Be`BL�BPB
�bB
ZB
B�B
�B	�B	�BB	�
B	ȴB	�^B	�VB	u�B	cTB	ZB	>wB	O�B	m�B	aHB	VB	H�B	>wB	VB	VB	R�B	N�B	H�B	/B	�B	�B	�B	uB	PB	+B	B��B�)B��B��BɺB��BÖB�RB�LB�FB�9B�-B�B�B�9B�XB�^B��B�}B�}B�wB�jB�dB�jB�jB�wB��B�}B�qB�dB�jB�qB�}BƨBȴBȴBŢBÖBĜB��B�B	uB	�B	-B	&�B	hB	1B��B�B��B	  B	�B	/B	B�B	`BB	t�B	�B	�B	r�B	x�B	�JB	�JB	�\B	�VB	�DB	�7B	�+B	�B	�JB	�VB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	�?B	�RB	�'B	�B	�FB	�}B	�wB	�wB	�qB	�qB	�wB	�}B	�wB	�qB	�dB	�^B	�^B	��B	ĜB	��B	�}B	�^B	�LB	�-B	�B	�B	�9B	�3B	B	ÖB	�FB	�LB	��B	�}B	��B	ĜB	��B	�XB	�qB	�}B	�LB	�LB	�XB	�LB	�?B	�?B	�?B	�FB	�FB	�FB	�^B	�LB	�RB	�RB	�dB	�^B	�qB	��B	��B	�}B	�}B	�}B	�}B	�}B	��B	B	ƨB	ŢB	ŢB	ƨB	ŢB	ŢB	ŢB	ƨB	ƨB	ƨB	ǮB	ƨB	ƨB	ŢB	ĜB	ÖB	ŢB	ɺB	��B	��B	��B	��B	��B	��B	�B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
%B
1B

=B
DB
DB
DB
DB
JB
JB
JB
JB
DB

=B
1B
+B
%B
B
B
B
B
%B
%B
%B
+B
1B
	7B
	7B
1B
1B
1B
+B
%B
1B
	7B
1B
1B
	7B
DB
JB
DB
DB

=B

=B

=B

=B

=B
	7B
DB
PB
PB
VB
VB
VB
VB
VB
VB
PB
JB
DB
DB

=B

=B
	7B
1B
1B
+B
+B
+B
%B
%B
%B
B
B
B
B
B
%B
%B
%B
%B
B
%B
%B
%B
+B
1B

=B

=B

=B
DB
VB
bB
\B
\B
\B
bB
hB
hB
hB
hB
hB
oB
oB
oB
oB
uB
uB
bB
uB
�B
 �B
%�B
0!B
33B
<jB
?}B
C�B
I�B
M�B
XB
]/B
bNB
hsB
l�B
q�B
u�B
y�B
|�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B	m�B	m�B	m�B	m�B	m�B	m�B	m�B	m�B	m�B	m�B	m�B	n�B	n�B	n�B	n�B	n�B	n�B	n�B	n�B	n�B	n�B	n�B	o�B	p�B	p�B	p�B	q�B	q�B	r�B	s�B	s�B	t�B	u�B	u�B	v�B	w�B	x�B	y�B	y�B	z�B	{�B	{�B	}�B	� B	�B	�B	�+B	�oB
�B
|�B
�oB
�DB
t�B
�B
�B
~�B
}�B
q�B
�bB
�LB
��B�Bo�B�DBk�BW
B!�B
��B
aHB
I�B
-B	��B	�mB	�#B	��B	ŢB	��B	}�B	jB	dZB	B�B	Q�B	s�B	e`B	]/B	VB	C�B	[#B	YB	YB	YB	[#B	>wB	�B	�B	 �B	�B	VB		7B	DB��B�sB�)B�B��B�B��B�dB�XB�XB�LB�9B�-B�9B�RB�^B�}BÖBÖBBBB��B��B�qB�}B��B��B�}B�wB�wB�}B��BȴB��B��BȴBĜBB��B�mB	uB	 �B	2-B	.B	{B	PB	  B�B��B��B	�B	.B	A�B	_;B	s�B	�B	�=B	t�B	v�B	�VB	�PB	�bB	�bB	�PB	�=B	�1B	�B	�PB	�VB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	�?B	�^B	�9B	�B	�FB	��B	��B	�}B	�wB	�wB	��B	��B	�}B	�wB	�jB	�dB	�^B	��B	ǮB	ÖB	��B	�dB	�XB	�9B	�'B	�!B	�?B	�!B	ÖB	ɺB	�LB	�FB	ÖB	��B	��B	ŢB	ÖB	�^B	�wB	ĜB	�RB	�LB	�^B	�^B	�FB	�FB	�FB	�LB	�LB	�LB	�qB	�XB	�^B	�XB	�jB	�dB	�wB	��B	B	��B	��B	��B	��B	��B	B	B	ǮB	ƨB	ƨB	ǮB	ƨB	ƨB	ƨB	ǮB	ǮB	ǮB	ɺB	ǮB	ǮB	ƨB	ŢB	ŢB	ǮB	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�B	�
B	�B	��B	��B	�B	�B	�B	�B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
%B
+B
	7B
DB
DB
DB
DB
JB
PB
PB
PB
PB
JB
JB
	7B
1B
+B
%B
B
B
B
%B
+B
+B
+B
	7B

=B

=B
	7B
1B

=B
1B
+B
	7B

=B
	7B
	7B
	7B
JB
PB
JB
JB

=B
DB
DB
DB
DB

=B
DB
PB
VB
\B
\B
VB
\B
VB
\B
VB
PB
JB
JB
DB
DB

=B
	7B
	7B
1B
1B
1B
1B
+B
+B
%B
%B
B
%B
%B
+B
+B
+B
+B
%B
%B
%B
+B
+B
	7B
DB
DB

=B
DB
VB
hB
bB
bB
\B
bB
oB
oB
hB
hB
hB
oB
oB
oB
oB
{B
uB
hB
uB
�B
 �B
%�B
0!B
33B
<jB
?}B
C�B
I�B
M�B
XB
^5B
bNB
hsB
l�B
q�B
u�B
y�B
|�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<�t�<#�
<#�
<D��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<��
<#�
<#�
<#�
<�o<49X<#�
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
<T��<#�
<#�
<#�
<#�
<#�
<�t�<u<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<D��<D��<#�
<#�
<#�
<D��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.3 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201131250142012011312501420120113125014  AO  ARGQ                                                                        20111205112644  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111205112644  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120113125014  IP                  G�O�G�O�G�O�                