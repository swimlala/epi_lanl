CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-12-05 AOML 2.2 creation; 2015-12-13T23:15:33Z UW 3.1 conversion   
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901337 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               "A   AO  20111205112904  20190522121836  1901_5055_034                   2C  D   APEX                            2140                            040306                          846 @�}�=��1   @�}��s��@.ݲ-V�c;dZ�1   GPS     Primary sampling: mixed [deeper than nominal 990dbar: discrete; nominal 990dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�ff@�  A   A   A@  A`  A�  A�  A�  A�33A�33A�33A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BXffB`ffBhffBp  Bw��B�  B�  B�  B�  B�  B�33B�ffB���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C��3C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#fD#� D$fD$� D%  D%� D&  D&� D'  D'� D(fD(�fD)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD�fDEfDE�fDF  DF� DGfDG�fDH  DH� DI  DI� DJ  DJy�DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS�fDT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dhy�Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dy�3D��fD�FfD�vfD�� D���D�6fD�ffD�� D�� D�  D�l�D�� D���D�33D�i�D��3D���D��D�ffD�� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @y��@�ff@�ffA33A;33A[33A{33A���A���A���A���A���Aݙ�A홚A���B��B��B��B��B&��B.��B6��B>��BF��BN��BW33B_33Bg33Bn��BvffB~��B�ffB�ffB�ffB�ffB���B���B�33B�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffC�3C�3C�3C�3C	�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C!�3C#�3C%�3C'�3C)�3C+�3C-�3C/�3C1�3C3�3C5�3C7�3C9�3C;�3C=�3C?�3CA�3CC�3CE�3CG�3CI�3CK�3CM�3CO�3CQ�3CS�3CU�3CW�3CY�3C[�3C]�3C_�3Ca�3Cc�3Ce�3Cg�3Ci�3Ck�3Cm�3Co�3Cq�3Cs�3Cu�3Cw�3Cy�3C{�3C}�3C�3C�ٚC�ٚC�ٚC��fC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC��fC�ٚC�ٚC�ٚC�ٚC���C���C���C�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC��fC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC��fC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚD l�D ��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��D	l�D	��D
l�D
��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��D l�D ��D!l�D!��D"l�D"�3D#l�D#�3D$l�D$��D%l�D%��D&l�D&��D'l�D'�3D(s3D(��D)l�D)��D*l�D*��D+l�D+��D,l�D,��D-l�D-��D.l�D.��D/l�D/��D0l�D0��D1l�D1��D2l�D2��D3l�D3��D4l�D4��D5l�D5��D6l�D6��D7l�D7��D8l�D8��D9l�D9��D:l�D:��D;l�D;��D<l�D<��D=l�D=��D>l�D>��D?l�D?��D@l�D@��DAl�DA��DBl�DB��DCl�DC��DDs3DD�3DEs3DE��DFl�DF�3DGs3DG��DHl�DH��DIl�DI��DJffDJ��DKl�DK��DLl�DL��DMl�DM��DNl�DN��DOl�DO��DPl�DP��DQl�DQ��DRl�DR��DSs3DS��DTl�DT��DUl�DU��DVl�DV��DWl�DW��DXl�DX��DYl�DY��DZl�DZ��D[l�D[��D\l�D\��D]l�D]��D^l�D^��D_l�D_��D`l�D`��Dal�Da��Dbl�Db��Dcl�Dc��Ddl�Dd��Del�De��Dfl�Df��Dgl�Dg��DhffDh��Dil�Di��Djl�Dj��Dkl�Dk��Dll�Dl��Dml�Dm��Dnl�Dn��Dol�Do��Dpl�Dp��Dql�Dq��Drl�Dr��Dsl�Ds��Dtl�Dt��Dul�Du��Dvl�Dv��Dy� D���D�<�D�l�D��fD�� D�,�D�\�D��fD��fD�fD�c3DǶfD��3D�)�D�` D๚D��3D� D�\�D�vf1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��Aʧ�Aʩ�AʬAʬAʮAʬAʬAʰ!Aʲ-Aʲ-Aʲ-Aʥ�Aʧ�Aʩ�Aʩ�Aʥ�Aʩ�AʬAʝ�Aʟ�Aʡ�Aʙ�Aʧ�Aʟ�Aʕ�AʓuAʓuAʋDA�ffAɶFAƾwA��mAħ�A�bNA�7LA�A�`BA�ȴA��A���A��DA���A�\)A�-A���A��RA��A�\)A��^A�hsA��A�oA�+A��A�ZA�jA���A�ȴA�%A�^5A���A��A� �A�A�A��A��TA��/A�  A�O�A�O�A���A���A�S�A�VA��-A�p�A���A��#A���A�G�A��mA�E�A��jA�I�A�r�A��HA��!A�A�A�ƨA�n�A���A��A�M�A�l�A�~�A���A�~�A�C�A�M�A�
A{\)Aw�At�9Aq��Ak
=Ad��Aa�A\I�AYdZAV-ATAR��AQVAO\)ANM�AM?}AK��AH��AGoAF�AFZAD�9ABr�A?��A=A:�RA7�;A4�yA1�A09XA. �A,��A+S�A)�-A(ĜA(JA(�RA)oA(bNA'p�A%ƨA#dZA"�A!�-A I�A �DA!�A!x�A!�A!�^A!ƨA"  A!�FA!|�A!+A ��A ^5A�A��A��A$�A�uA�mAXAȴA  A\)A
=AdZA��A�A��A/A�yA�jA��A��AA�A�A5?A{A�
A1An�A��AoA7LA;dA7LA�A~�A\)A/A
=A�`A1'A��A%A(�A{AA�7AK�A�/A�\A1'AQ�A��A��A��AS�A
�yA
��A
�RA
9XA	A	��A�A��A�mA��A��A7LA�yA�\A�Ap�A�/AE�A�AG�A`BA�A ��@��@�V@��u@��@��`@��@��@���@�p�@�7L@��D@�X@�V@���@���@�5?@��h@���@�bN@���@��@�~�@�J@�p�@�&�@�z�@�ƨ@��@���@��@�$�@�@�%@�@��m@�@�"�@�\@홚@�&�@���@�j@�9X@��m@�K�@�S�@��y@�+@���@��@�(�@�v�@�^@�O�@���@���@�bN@��H@��@�7@�O�@�G�@�Z@���@�K�@އ+@��#@ݩ�@�?}@��@�j@�t�@�C�@ڗ�@���@�&�@��@ج@�1@׍P@�
=@֧�@�=q@���@�/@��@�1'@ӍP@ҟ�@��@Ѻ^@���@Ь@�1'@�r�@Ь@Гu@�Z@�  @�K�@θR@�@Ͳ-@��@���@���@��#@͡�@�X@�&�@̬@�Z@�;d@�n�@�ff@�x�@ȴ9@�z�@�  @Ǿw@�33@���@ƸR@�E�@őh@�V@Ĵ9@�I�@��@��;@î@�S�@�;d@�
=@+@�5?@��#@�p�@��@��j@�Z@��
@�\)@�o@�V@�$�@���@���@���@���@��@��\@�^5@�J@��#@��-@�p�@�Ĝ@�r�@���@�M�@���@�J@���@��;@��@�A�@�Q�@�  @��@�hs@��j@�z�@��@���@�v�@���@�V@���@� �@�\)@��@��!@�^5@���@�7L@���@�Ĝ@�z�@�(�@���@�33@��@��y@���@��@��@��^@���@���@�`B@�/@��@���@�(�@��;@�|�@�+@���@�v�@�M�@�-@���@���@�X@���@�j@��@��w@�t�@�;d@���@���@�ff@�^5@���@��h@�?}@�%@��@��j@��@�Q�@��@��w@�t�@�"�@�
=@��y@���@�E�@��T@��7@�/@�Ĝ@�Q�@��m@��@�l�@�
=@���@�ff@�$�@�@��@���@�x�@�G�@��@���@�Ĝ@���@�%@x�u@q�@g�w@^�+@V��@P �@G�@A�7@:�@3t�@,��@'K�@"J@�@;d@�H@v�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  Aʧ�Aʩ�AʬAʬAʮAʬAʬAʰ!Aʲ-Aʲ-Aʲ-Aʥ�Aʧ�Aʩ�Aʩ�Aʥ�Aʩ�AʬAʝ�Aʟ�Aʡ�Aʙ�Aʧ�Aʟ�Aʕ�AʓuAʓuAʋDA�ffAɶFAƾwA��mAħ�A�bNA�7LA�A�`BA�ȴA��A���A��DA���A�\)A�-A���A��RA��A�\)A��^A�hsA��A�oA�+A��A�ZA�jA���A�ȴA�%A�^5A���A��A� �A�A�A��A��TA��/A�  A�O�A�O�A���A���A�S�A�VA��-A�p�A���A��#A���A�G�A��mA�E�A��jA�I�A�r�A��HA��!A�A�A�ƨA�n�A���A��A�M�A�l�A�~�A���A�~�A�C�A�M�A�
A{\)Aw�At�9Aq��Ak
=Ad��Aa�A\I�AYdZAV-ATAR��AQVAO\)ANM�AM?}AK��AH��AGoAF�AFZAD�9ABr�A?��A=A:�RA7�;A4�yA1�A09XA. �A,��A+S�A)�-A(ĜA(JA(�RA)oA(bNA'p�A%ƨA#dZA"�A!�-A I�A �DA!�A!x�A!�A!�^A!ƨA"  A!�FA!|�A!+A ��A ^5A�A��A��A$�A�uA�mAXAȴA  A\)A
=AdZA��A�A��A/A�yA�jA��A��AA�A�A5?A{A�
A1An�A��AoA7LA;dA7LA�A~�A\)A/A
=A�`A1'A��A%A(�A{AA�7AK�A�/A�\A1'AQ�A��A��A��AS�A
�yA
��A
�RA
9XA	A	��A�A��A�mA��A��A7LA�yA�\A�Ap�A�/AE�A�AG�A`BA�A ��@��@�V@��u@��@��`@��@��@���@�p�@�7L@��D@�X@�V@���@���@�5?@��h@���@�bN@���@��@�~�@�J@�p�@�&�@�z�@�ƨ@��@���@��@�$�@�@�%@�@��m@�@�"�@�\@홚@�&�@���@�j@�9X@��m@�K�@�S�@��y@�+@���@��@�(�@�v�@�^@�O�@���@���@�bN@��H@��@�7@�O�@�G�@�Z@���@�K�@އ+@��#@ݩ�@�?}@��@�j@�t�@�C�@ڗ�@���@�&�@��@ج@�1@׍P@�
=@֧�@�=q@���@�/@��@�1'@ӍP@ҟ�@��@Ѻ^@���@Ь@�1'@�r�@Ь@Гu@�Z@�  @�K�@θR@�@Ͳ-@��@���@���@��#@͡�@�X@�&�@̬@�Z@�;d@�n�@�ff@�x�@ȴ9@�z�@�  @Ǿw@�33@���@ƸR@�E�@őh@�V@Ĵ9@�I�@��@��;@î@�S�@�;d@�
=@+@�5?@��#@�p�@��@��j@�Z@��
@�\)@�o@�V@�$�@���@���@���@���@��@��\@�^5@�J@��#@��-@�p�@�Ĝ@�r�@���@�M�@���@�J@���@��;@��@�A�@�Q�@�  @��@�hs@��j@�z�@��@���@�v�@���@�V@���@� �@�\)@��@��!@�^5@���@�7L@���@�Ĝ@�z�@�(�@���@�33@��@��y@���@��@��@��^@���@���@�`B@�/@��@���@�(�@��;@�|�@�+@���@�v�@�M�@�-@���@���@�X@���@�j@��@��w@�t�@�;d@���@���@�ff@�^5@���@��h@�?}@�%@��@��j@��@�Q�@��@��w@�t�@�"�@�
=@��y@���@�E�@��T@��7@�/@�Ĝ@�Q�@��m@��@�l�@�
=@���@�ff@�$�@�@��@���@�x�@�G�@��@���@�Ĝ@���@�%@x�u@q�@g�w@^�+@V��@P �@G�@A�7@:�@3t�@,��@'K�@"J@�@;d@�H@v�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
�FB
�FB
�FB
�FB
�FB
�FB
�FB
�FB
�FB
�FB
�?B
�?B
�?B
�?B
�?B
�?B
�?B
�?B
�9B
�9B
�9B
�9B
�?B
�9B
�9B
�9B
�9B
�9B
�9B
�FB
�NB �B&�B/B6FB:^BD�BK�B`BBy�B� B�PBB�B,BF�BQ�Bq�B�B�DB��B�DB�DB��B�B��B� B}�B�B�B�+B�PB�VB�1B�B�oB��B�hB�%By�B}�B}�B{�Bs�Bm�Bm�BffB[#BO�B>wB.B'�B�BVBB��B��B�B�B�RB�{Bn�B;dB1B
�sB
��B
�PB
e`B
/B	��B	��B	�^B	��B	�+B	YB	0!B	�B	B��B��B��B��B	B	+B		7B	DB	\B	oB	hB	\B	PB		7B	B	  B	B	DB	1B��B��B�B�B�B�fB�fB��B��B	{B	$�B	�B	�B	�B	�B	{B	{B	�B	A�B	k�B	q�B	y�B	�=B	�JB	�oB	�uB	��B	��B	��B	��B	�'B	�FB	�-B	��B	��B	�=B	�1B	�7B	iyB	gmB	k�B	�B	�B	�B	�JB	�VB	�VB	�uB	��B	��B	��B	��B	�3B	�LB	�LB	�jB	ǮB	��B	�5B	�`B	�yB	�B	�B	�sB	�fB	�B	�B	�B	�mB	�BB	�yB	��B
B
+B
%B
%B
B
B
B
PB
VB
bB
oB
oB
hB
hB
uB
uB
hB
oB
bB
hB

=B	��B
  B	��B	��B	��B	��B	�B	�B	�B	�mB	�sB	�B	�B	�mB	�BB	�)B	�#B	�fB	�yB	�mB	�B	��B	��B	��B	��B
JB
PB
DB
+B
B
B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�yB	�fB	�`B	�`B	�ZB	�ZB	�NB	�TB	�ZB	�ZB	�ZB	�ZB	�TB	�NB	�HB	�BB	�BB	�BB	�BB	�HB	�BB	�BB	�BB	�BB	�BB	�HB	�HB	�HB	�NB	�HB	�NB	�TB	�TB	�TB	�NB	�NB	�NB	�HB	�HB	�HB	�ZB	�ZB	�`B	�mB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B	��B	��B
  B
  B
  B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
  B	��B	��B
B
  B	��B	��B	��B
B
B
B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
%B
%B
+B
+B
+B
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

=B
\B
�B
$�B
1'B
49B
:^B
?}B
D�B
K�B
Q�B
VB
ZB
_;B
bNB
ffB
k�B
p�B
t�B
x�B
|�B
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
�FB
�FB
�FB
�FB
�FB
�FB
�FB
�FB
�FB
�FB
�?B
�?B
�?B
�?B
�?B
�?B
�?B
�?B
�9B
�9B
�9B
�9B
�?B
�9B
�9B
�9B
�9B
�9B
�?B
�jB
�sB!�B'�B0!B7LB=qBE�BN�BcTBz�B�B�oBÖB�B0!BI�BT�Bt�B�B�PB��B�bB�PB�B�3B��B�+B�B�B�7B�DB�oB�oB�VB�B�oB��B�{B�=B|�B�B�%B�Bv�Bo�Bq�Bk�B`BBZBF�B2-B,B(�BuB+B  B��B��B�5B��B��By�BF�BbB
�B
��B
��B
r�B
?}B
B	�5B	B	��B	�uB	cTB	5?B	!�B	+B	  B��B��B	  B	+B	
=B	JB	bB	�B	�B	oB	hB	oB	\B	VB	%B	DB	{B	oB		7B��B��B�B�B�B�sB��B��B	{B	&�B	!�B	 �B	 �B	�B	�B	�B	�B	>wB	l�B	q�B	y�B	�=B	�JB	�uB	�{B	��B	��B	��B	��B	�-B	�RB	�?B	�B	��B	�JB	�DB	��B	k�B	hsB	jB	�%B	�+B	�B	�VB	�\B	�\B	�uB	��B	��B	��B	��B	�9B	�RB	�LB	�dB	ƨB	��B	�5B	�`B	�yB	�B	�B	�B	�mB	�B	�B	�B	�B	�;B	�fB	��B
B
	7B
+B
1B
%B
%B
B
\B
\B
bB
uB
{B
oB
hB
�B
�B
oB
{B
hB
{B
bB	��B
B	��B	��B	��B	��B	�B	�B	�B	�sB	�sB	�B	�B	�B	�NB	�/B	�B	�mB	�B	�mB	�B	��B	��B	��B	��B
PB
VB
PB
	7B
B
B
  B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�mB	�fB	�fB	�ZB	�`B	�`B	�ZB	�`B	�`B	�ZB	�fB	�ZB	�TB	�NB	�HB	�HB	�HB	�HB	�NB	�NB	�HB	�HB	�HB	�HB	�NB	�NB	�NB	�TB	�NB	�TB	�ZB	�ZB	�ZB	�TB	�TB	�TB	�TB	�NB	�NB	�ZB	�`B	�`B	�mB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
  B
  B
B
B
B	��B
  B	��B	��B	��B
  B	��B
  B
B
B
B
B
B
B
  B	��B	��B
B
  B	��B	��B	��B
B
B
B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
+B
+B
+B
+B
1B
1B
1B
1B
1B
1B
1B
	7B
	7B

=B
	7B
	7B
	7B
DB
DB
DB

=B
bB
�B
%�B
1'B
49B
:^B
?}B
E�B
K�B
Q�B
W
B
ZB
_;B
cTB
ffB
k�B
p�B
u�B
x�B
|�B
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<T��<#�
<T��<�o<49X<#�
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
<u<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.3 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201131250192012011312501920120113125019  AO  ARGQ                                                                        20111205112904  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111205112904  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120113125019  IP                  G�O�G�O�G�O�                