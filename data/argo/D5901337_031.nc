CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-12-05 AOML 2.2 creation; 2015-12-13T23:15:32Z UW 3.1 conversion   
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
_FillValue        G�O�   axis      Z          9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�       C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�       M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       i�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       q�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   y�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       {�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �8   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �<   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �@   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �D   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �H   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901337 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               A   AO  20111205112839  20190522121836  1901_5055_031                   2C  D   APEX                            2140                            040306                          846 @�v[����1   @�v\K� 	@.���R�c'�vȴ91   GPS     Primary sampling: mixed [deeper than nominal 990dbar: discrete; nominal 990dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8ffB@ffBH  BP  BX  B`  Bh  Bp  Bx  B�  B�ffB���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  C   C�fC  C  C  C
  C  C  C  C  C  C  C�fC�fC  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb�Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\fD\�fD]  D]� D^  D^� D_  D_� D`  D`� Da  Day�Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds�fDtfDt�fDu  Du� Dv  Dv� Dy�fD�fD�)�D�i�D���D���D�<�D�ffD���D��fD�#3D���D�ɚD��fD�&fDډ�D��3D��3D��D�Y�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @&ff@l��@�ff@�ffA33A;33A[33A{33A���A���A���A���A͙�Aݙ�A홚A���B��B��B��B��B&��B.��B733B?33BF��BN��BV��B^��Bf��Bn��Bv��B~��B���B�  B�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�33B�ffB�ffB�ffB�ffC��C�3C�3C�3C	�3C�3C�3C�3C�3C�3C�3C��C��C�3C�3C�3C!�3C#�3C%�3C'�3C)�3C+�3C-�3C/�3C1�3C3�3C5�3C7�3C9�3C;�3C=�3C?�3CA�3CC�3CE�3CG�3CI�3CK�3CM�3CO�3CQ�3CS�3CU�3CW�3CY�3C[�3C]�3C_�3Ca��Cc�3Ce�3Cg�3Ci�3Ck�3Cm�3Co�3Cq�3Cs�3Cu�3Cw�3Cy�3C{�3C}�3C�3C�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚD l�D ��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��D	l�D	��D
l�D
��Dl�D��Dl�D��Dl�D��Dl�D��DffD��Dl�D��Dl�D�fDl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��D l�D ��D!l�D!��D"l�D"��D#l�D#��D$l�D$��D%l�D%��D&l�D&��D'l�D'��D(l�D(��D)l�D)��D*l�D*��D+l�D+��D,l�D,��D-l�D-��D.l�D.��D/l�D/��D0l�D0��D1l�D1��D2l�D2��D3l�D3��D4l�D4��D5l�D5��D6l�D6��D7l�D7��D8l�D8��D9l�D9��D:l�D:��D;l�D;��D<l�D<��D=l�D=��D>l�D>��D?l�D?��D@l�D@��DAl�DA��DBl�DB��DCl�DC��DDl�DD��DEl�DE��DFl�DF��DGl�DG��DHl�DH��DIl�DI��DJl�DJ��DKl�DK��DLl�DL��DMl�DM��DNl�DN��DOl�DO��DPl�DP��DQl�DQ��DRl�DR��DSl�DS��DTl�DT��DUl�DU��DVl�DV��DWl�DW��DXl�DX��DYl�DY��DZl�DZ��D[l�D[�3D\s3D\��D]l�D]��D^l�D^��D_l�D_��D`l�D`��DaffDa��Dbl�Db��Dcl�Dc��Ddl�Dd��Del�De��Dfl�Df��Dgl�Dg��Dhl�Dh��Dil�Di��Djl�Dj��Dkl�Dk��Dll�Dl��Dml�Dm��Dnl�Dn��Dol�Do��Dpl�Dp��Dql�Dq��Drl�Dr��Dss3Ds�3Dts3Dt��Dul�Du��Dvl�Dy�3D���D�  D�` D��3D�� D�33D�\�D�� D���D��D�� D�� D���D��Dڀ D๚D��D�3D�P 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�+A�+A�-A�33A�33A�7LA�9XA�=qA�?}A�?}A�=qA�?}A�;dA�=qA�?}A�A�A�C�A�C�A�E�A�G�A�;dA�1'A�9XA�;dA�I�A���AȲ-A�5?A�v�A���A�;dA�z�AāA�ƨA��A���A��A�ZA��A���A��7A�z�A�x�A���A�dZA��TA�VA�A��A���A�7LA�r�A��yA�M�A���A��A�hsA��PA�"�A��A��
A�A�dZA�I�A�5?A�-A��;A�l�A��A�l�A���A�\)A��A�^5A��HA�=qA�+A��HA�S�A�=qA���A�$�A��jA�{A�z�A��A��FA�"�A���A��FA�hsA�dZA�C�A�ȴA��^A�A�A�l�A���A�A�jA�ȴA�|�A���A|-AudZAshsAqXAk�FAi��Agp�AaC�A]��A[%AXQ�AW"�AU�FAO�7AM�hAKdZAGS�AD�uA@�`A>�A<��A;x�A:(�A8��A8��A6I�A2�A0��A.�A,�+A*��A)��A(��A(ZA'ƨA'A&=qA%�A%+A$�A#/A!x�A�`A��A�wA�-At�A~�A��A�\A7LA�A&�AZA5?A�A�A|�A��A�;A��A��AA�AXA��A��A��A��AC�A�AE�A$�AVAM�A$�A=qA�wA��A33A�A�+A��A�A�A�A�FA�`A
=A	��AĜA��A�yA
�`A��A��A��A�PAO�AdZA�A
��A	�A	�-A	�mA	|�A	7LA	%AȴA~�AZAZA��A�AVAAE�A�7A�yAA�AS�A%A��A1A�-A%A n�@��m@��m@��H@���@��@��`@�Z@�1@�S�@���@��;@�M�@�^5@�ȴ@�ȴ@��@�Ĝ@�@�(�@���@�K�@�/@�9@�@��m@�S�@��@�ff@��`@땁@��@��@�J@�-@�`B@�@��@�1@�@�V@��#@��@�-@�X@�\)@�{@�@�{@�@�&�@��@��@��@�bN@߾w@ޏ\@�O�@��/@���@�1@�"�@ڏ\@���@���@���@ו�@�S�@ְ!@�Ĝ@�C�@�@�%@�I�@�Ĝ@���@�M�@��y@���@�x�@��@���@�p�@�X@�V@��H@�=q@�-@�-@�E�@��@�Ĝ@�b@˶F@�ƨ@˝�@�t�@��@�@���@�1@ǅ@�C�@�+@�@��@ŉ7@Ĵ9@�  @��@�5?@�&�@�G�@���@�x�@��@�Q�@�"�@�5?@���@���@��^@��7@��@�I�@���@���@���@�|�@�;d@��@�;d@��
@��F@�33@���@�@��@���@�hs@���@��@��@���@���@���@�%@��@�1'@��F@�+@�~�@�5?@��T@�G�@��@��u@�1@���@��@�t�@���@��@���@�&�@���@�j@��@�ƨ@���@�l�@�;d@��y@�ff@��@���@�x�@��@���@��u@�(�@� �@�  @��m@���@�"�@�ȴ@���@��@�@��@�/@��@���@�z�@��@���@�t�@�
=@��@���@�E�@��T@���@�hs@�O�@���@��D@�b@��P@��@��@��R@�v�@�E�@���@��@��9@�bN@�b@��@�"�@��!@�-@��@�?}@�Ĝ@��@��
@�|�@�S�@�33@�@���@�5?@���@��^@��@�/@���@��u@�bN@��m@���@�33@��R@�=q@��T@��^@���@��7@�`B@�V@���@��@�Z@�1'@��@�  @��m@��F@�33@���@�M�@�G�@��@�I�@;d@u`B@j�@b��@Y�@RJ@M��@Gl�@@Ĝ@;�@333@-?}@$9X@�@�!@V@G�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�+A�+A�-A�33A�33A�7LA�9XA�=qA�?}A�?}A�=qA�?}A�;dA�=qA�?}A�A�A�C�A�C�A�E�A�G�A�;dA�1'A�9XA�;dA�I�A���AȲ-A�5?A�v�A���A�;dA�z�AāA�ƨA��A���A��A�ZA��A���A��7A�z�A�x�A���A�dZA��TA�VA�A��A���A�7LA�r�A��yA�M�A���A��A�hsA��PA�"�A��A��
A�A�dZA�I�A�5?A�-A��;A�l�A��A�l�A���A�\)A��A�^5A��HA�=qA�+A��HA�S�A�=qA���A�$�A��jA�{A�z�A��A��FA�"�A���A��FA�hsA�dZA�C�A�ȴA��^A�A�A�l�A���A�A�jA�ȴA�|�A���A|-AudZAshsAqXAk�FAi��Agp�AaC�A]��A[%AXQ�AW"�AU�FAO�7AM�hAKdZAGS�AD�uA@�`A>�A<��A;x�A:(�A8��A8��A6I�A2�A0��A.�A,�+A*��A)��A(��A(ZA'ƨA'A&=qA%�A%+A$�A#/A!x�A�`A��A�wA�-At�A~�A��A�\A7LA�A&�AZA5?A�A�A|�A��A�;A��A��AA�AXA��A��A��A��AC�A�AE�A$�AVAM�A$�A=qA�wA��A33A�A�+A��A�A�A�A�FA�`A
=A	��AĜA��A�yA
�`A��A��A��A�PAO�AdZA�A
��A	�A	�-A	�mA	|�A	7LA	%AȴA~�AZAZA��A�AVAAE�A�7A�yAA�AS�A%A��A1A�-A%A n�@��m@��m@��H@���@��@��`@�Z@�1@�S�@���@��;@�M�@�^5@�ȴ@�ȴ@��@�Ĝ@�@�(�@���@�K�@�/@�9@�@��m@�S�@��@�ff@��`@땁@��@��@�J@�-@�`B@�@��@�1@�@�V@��#@��@�-@�X@�\)@�{@�@�{@�@�&�@��@��@��@�bN@߾w@ޏ\@�O�@��/@���@�1@�"�@ڏ\@���@���@���@ו�@�S�@ְ!@�Ĝ@�C�@�@�%@�I�@�Ĝ@���@�M�@��y@���@�x�@��@���@�p�@�X@�V@��H@�=q@�-@�-@�E�@��@�Ĝ@�b@˶F@�ƨ@˝�@�t�@��@�@���@�1@ǅ@�C�@�+@�@��@ŉ7@Ĵ9@�  @��@�5?@�&�@�G�@���@�x�@��@�Q�@�"�@�5?@���@���@��^@��7@��@�I�@���@���@���@�|�@�;d@��@�;d@��
@��F@�33@���@�@��@���@�hs@���@��@��@���@���@���@�%@��@�1'@��F@�+@�~�@�5?@��T@�G�@��@��u@�1@���@��@�t�@���@��@���@�&�@���@�j@��@�ƨ@���@�l�@�;d@��y@�ff@��@���@�x�@��@���@��u@�(�@� �@�  @��m@���@�"�@�ȴ@���@��@�@��@�/@��@���@�z�@��@���@�t�@�
=@��@���@�E�@��T@���@�hs@�O�@���@��D@�b@��P@��@��@��R@�v�@�E�@���@��@��9@�bN@�b@��@�"�@��!@�-@��@�?}@�Ĝ@��@��
@�|�@�S�@�33@�@���@�5?@���@��^@��@�/@���@��u@�bN@��m@���@�33@��R@�=q@��T@��^@���@��7@�`B@�V@���@��@�Z@�1'@��@�  @��m@��F@�33@���@�M�@�G�@��@�I�@;d@u`B@j�@b��@Y�@RJ@M��@Gl�@@Ĝ@;�@333@-?}@$9X@�@�!@V@G�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
9XB
9XB
9XB
9XB
9XB
8RB
8RB
9XB
9XB
9XB
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
9XB
9XB
9XB
8RB
<jB
A�B
M�B
m�B
�PB
��B
�-B
��B
�B+BD�BXBo�B�JB�hB�B�1BVBoB�B!�B+B8RBH�BI�BVB^5BaHBgmB_;BZBgmB|�B�hB��B��B��B�oB��B��B��B��B��B��B��B��B��B��B�uB�\B�PB�B}�Bt�BcTBL�BF�B6FB,B"�B�BhBB�B�BÖB�'B��Bk�BE�B&�B
�B
ǮB
�FB
��B
s�B
G�B
�B	�;B	�B	��B	� B	]/B	K�B	9XB	#�B	�B	�B	bB	
=B	
=B	�B	�B	�B	�B	{B	�B	#�B	'�B	 �B	�B	�B	�B	�B	oB	B��B	  B��B��B��B	
=B	�B	&�B	.B	2-B	F�B	[#B	ZB	Q�B	D�B	:^B	:^B	?}B	D�B	I�B	G�B	B�B	:^B	8RB	<jB	9XB	o�B	�PB	��B	��B	�B	�-B	�LB	��B	�B	��B	�B	��B	ǮB	B	��B	��B	�)B	�fB	�`B	�)B	�B	�5B	�;B	�#B	�
B	�;B	�NB	�sB	�`B	�NB	�5B	�B	�B	��B	ĜB	�jB	�LB	�XB	�wB	�sB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
+B
	7B
	7B

=B
DB
VB
uB
hB
VB
bB
oB
VB

=B
+B
B
B
B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	�B	�B	�B	��B	��B	��B	�B	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�sB	�fB	�ZB	�fB	�mB	�yB	�sB	�B	�B	�B	�B	�B	�sB	�sB	�B	�B	�B	�B	�B	�B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�yB	�mB	�mB	�ZB	�5B	�B	�
B	�B	�B	�)B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�sB	�`B	�yB	�B	�B	�B	�B	�mB	�ZB	�ZB	�TB	�TB	�NB	�HB	�HB	�fB	�mB	�mB	�mB	�fB	�fB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
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
1B
1B
	7B

=B

=B

=B

=B
DB
DB
JB
JB
JB
JB
JB
PB
VB
VB
VB
\B
bB
bB
hB
hB
uB
uB
{B
{B
{B
�B
�B
�B
�B
�B
�B
!�B
,B
2-B
9XB
?}B
C�B
I�B
P�B
S�B
YB
]/B
aHB
cTB
ffB
o�B
r�B
u�B
y�B
}�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
9XB
9XB
9XB
9XB
9XB
8RB
8RB
9XB
9XB
9XB
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
9XB
9XB
9XB
9XB
=qB
B�B
O�B
p�B
�\B
��B
�FB
��B
�/B-BF�BZBq�B�bB��B�B�1BhB�B�B$�B-B8RBI�BL�BZBaHBdZBn�BbNB\)BjB~�B�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB�uB�B�B{�Bm�BO�BO�B:^B/B$�B�BuB
=B�B�/BȴB�FB��Bu�BM�B6FBB
��B
�}B
��B
�B
XB
)�B	�B	�-B	��B	�bB	cTB	R�B	K�B	/B	$�B	�B	uB	VB	�B	"�B	#�B	'�B	 �B	�B	�B	(�B	,B	$�B	�B	�B	$�B	 �B	�B		7B	B	B��B��B	  B	JB	�B	(�B	0!B	49B	G�B	`BB	_;B	YB	J�B	=qB	:^B	@�B	G�B	K�B	K�B	F�B	>wB	;dB	>wB	33B	m�B	�DB	��B	��B	�B	�-B	�9B	��B	�)B	��B	�)B	��B	ɺB	ÖB	��B	��B	�#B	�mB	�sB	�5B	�#B	�5B	�HB	�/B	�
B	�BB	�ZB	�B	�fB	�ZB	�HB	�B	�#B	�
B	ȴB	�}B	�LB	�RB	�RB	�fB	��B	��B	��B	��B	��B
  B
  B	��B	��B	��B
%B
1B

=B

=B
DB
JB
VB
�B
uB
VB
hB
�B
hB
JB
	7B
1B
B
B
B	��B	��B	��B	��B	�B	��B	�B	�B	�B	��B	��B	��B	��B	��B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�yB	�mB	�`B	�fB	�sB	�B	�yB	�B	�B	�B	��B	�B	�yB	�sB	�B	�B	�B	�B	�B	�B	��B	��B	��B	�B	�B	��B	�B	�B	�B	�B	�B	�B	�sB	�sB	�mB	�HB	�/B	�B	�
B	��B	�B	�B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�`B	�yB	�B	�B	�B	�B	�yB	�`B	�ZB	�TB	�ZB	�TB	�NB	�HB	�mB	�mB	�sB	�sB	�fB	�fB	�mB	�B	�B	�B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
+B
+B
%B
1B
1B
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
DB
DB
DB
DB
JB
JB
PB
PB
PB
PB
PB
VB
\B
VB
VB
\B
hB
hB
oB
hB
uB
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
!�B
,B
2-B
9XB
?}B
C�B
J�B
P�B
S�B
YB
^5B
aHB
cTB
ffB
o�B
r�B
u�B
y�B
}�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<u<e`B<#�
<#�
<#�
<D��<u<��
<�t�<#�
<#�
<�o<#�
<#�
<�C�<#�
<#�
<#�
<#�
<#�
<e`B<#�
<#�
<49X<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.3 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201131250182012011312501820120113125018  AO  ARGQ                                                                        20111205112839  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111205112839  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120113125018  IP                  G�O�G�O�G�O�                