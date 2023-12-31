CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901072 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               "A   AO  20111130143831  20190522121828  1728_5048_034                   2C  D   APEX                            2142                            040306                          846 @�W�r@1   @�X/h`@5[��S���cGKƧ�1   GPS     Primary sampling: mixed [deeper than nominal 990dbar: discrete; nominal 990dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C�C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV�CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cw�fCz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,fD,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D5��D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  DvffDyL�D�	�D�C3D�vfD��fD���D�)�D�i�D���D���D�&fD�` D�ɚD��D�3D�s3D�fD���D�  D�P D�#3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�  @�ff@�ffA33A;33A[33A{33A���A���A���A���A͙�Aݙ�A홚A���B��B��B��B��B&��B.��B6��B>��BF��BN��BV��B^��Bf��Bn��Bv��B~��B�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffC�3C�3C�3C�3C	�3C�3C�3C�3C�3C�3C��C�3C�3C�3C�3C�3C!�3C#�3C%�3C'�3C)�3C+�3C-�3C/�3C1�3C3�3C5�3C7�3C9�3C;�3C=�3C?�3CA�3CC�3CE�3CG�3CI�3CK�3CM�3CO�3CQ�3CS�3CU��CW�3CY�3C[�3C]�3C_�3Ca�3Cc�3Ce�3Cg�3Ci�3Ck�3Cm�3Co�3Cq�3Cs�3Cu�3Cw��Cy�3C{�3C}�3C�3C�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC��fC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC���C�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC���C�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚD l�D ��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��D	l�D	��D
l�D
��Dl�D��Dl�D��DffD��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D�3Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��D l�D ��D!l�D!��D"l�D"��D#l�D#��D$l�D$��D%l�D%��D&l�D&��D'l�D'��D(l�D(��D)l�D)��D*l�D*��D+l�D+�3D,l�D,��D-l�D-��D.l�D.��D/l�D/��D0l�D0��D1l�D1��D2l�D2��D3l�D3��D4l�D4��D5l�D5�fD6l�D6��D7l�D7��D8l�D8��D9l�D9��D:l�D:��D;l�D;��D<l�D<��D=l�D=��D>l�D>��D?l�D?��D@l�D@��DAl�DA��DBl�DB��DCl�DC��DDl�DD��DEl�DE��DFl�DF��DGl�DG��DHl�DH��DIl�DI��DJl�DJ��DKl�DK��DLl�DL��DMl�DM��DNl�DN��DOl�DO��DPl�DP��DQl�DQ��DRl�DR��DSl�DS��DTl�DT��DUl�DU��DVl�DV��DWl�DW��DXl�DX��DYl�DY��DZl�DZ��D[l�D[��D\l�D\��D]l�D]��D^l�D^��D_l�D_��D`l�D`��Dal�Da��Dbl�Db��Dcl�Dc��Ddl�Dd��Del�De��Dfl�Df��Dgl�Dg��Dhl�Dh��Dil�Di��Djl�Dj��Dkl�Dk��Dll�Dl��Dml�Dm��Dnl�Dn��Dol�Do��Dpl�Dp��Dql�Dq��Drl�Dr��Dsl�Ds��Dtl�Dt��Dul�Du��DvS3Dy9�D�  D�9�D�l�D���D��3D�  D�` D��3D�� D��D�VfD�� D�� D�	�D�i�D���D��3D�fD�FfD��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A��A��A��A��A��A��A��mA��;A��/A���AƝ�AƏ\A�VA�A�A�=qA�9XA�-A�"�A�oA�A���A��A��A��`A���AŴ9A�VA��A�1A�hsA��A�\)A�&�A�
=A��mA��^A��PA�9XA�JA�p�A���A�-A��A�t�A�bA�ƨA�VA�  A��PA�dZA�&�A��A���A�x�A�K�A���A�\)A���A��A�ȴA��A���A��A���A��A��A�+A��A�oA�t�A�&�A���A���A�n�A�VA���A�x�A��A���A��A��#A���A��FA�p�A���A�;dA���A�|�A���A��A��/A���A��A��A�  A�\)A�t�A��HA��uA�hsA���A��A�VA���A�  A�M�A�ZA�1'A��wA�ZA�&�A���A�t�A�ȴA���A�?}A��PA�p�A�{A���A��FA�  A�oA���A�|�A�v�A�VA�XA���A�;dA��A���A�ZA�+A}�wAz~�AxAuAs�^Ao�AmdZAi+Ac+A]\)AZM�AV�HAU�AQ��AM&�AJffAH��AF1'AD5?AB{A@r�A?\)A<�A9hsA8n�A7oA5��A4��A3�mA3`BA2��A1�wA0ffA/��A.��A-p�A,=qA+&�A*5?A'�A&9XA%�A$�A$bA"�RA!�A bNA��A�A�A��A;dA��A�AZA�A�FA�RA��AM�A�-AG�A��Ax�A
=A�FA��A(�A�hA"�AƨA%A�jAbA�wA�A
Q�An�A�/AƨA��An�Al�Av�AƨAO�A ��A J@�5?@�1@�J@�C�@��@�
=@�@�V@��@��@�$�@��@��@�O�@�P@�~�@�x�@䛦@�(�@�\)@�@�X@ޗ�@���@���@ݩ�@ݑh@݁@�O�@��`@�1@�l�@��@ו�@�{@Ձ@ԣ�@�1'@�C�@җ�@�v�@�V@�=q@�@�V@�l�@��@��T@�z�@��@�;d@�^5@�?}@��`@���@ț�@�z�@�Z@�1'@ǶF@Ɵ�@�=q@�@��@���@�%@�v�@��@���@���@�z�@�I�@�A�@�Q�@�j@���@���@��D@��@�A�@�ƨ@�\)@��@�@���@��7@�/@��D@�b@���@�t�@�V@��R@��@��w@���@�O�@� �@��@��@��@�"�@�ff@�?}@��`@��@�K�@��@�$�@��@�5?@�hs@�@���@�9X@�\)@�S�@��y@�E�@��+@�`B@�C�@�$�@�7L@���@�J@���@��@�K�@��\@�n�@�^5@��@���@��-@���@��h@��@��h@�Ĝ@��@�j@�9X@�b@�(�@�(�@� �@�1@��;@��@���@��@�|�@�|�@�|�@�t�@�t�@�t�@�t�@�t�@�dZ@�+@���@��\@�V@���@�G�@��u@�9X@�1@��w@�|�@��@��@��^@�%@��u@�b@��
@���@�S�@�o@���@���@�$�@��@��9@��j@��`@���@���@���@�&�@�/@�Ĝ@��P@���@���@���@�&�@�?}@���@�A�@��@��y@��\@�-@��T@��-@�x�@��@���@�ƨ@�"�@��@��^@��h@��^@��T@���@�x�@��@���@�Ĝ@�r�@�9X@���@��P@�\)@�"�@�ȴ@���@��\@�n�@�M�@�=q@�$�@��@��@�V@��j@��@��@���@��u@��@�Q�@��@���@���@���@���@��@�o@��+@�=q@�{@���@��@��@�@���@��7@�&�@���@��D@�z�@�j@�j@�33@~ff@st�@f��@^ff@Vff@Pr�@K33@B�@;@4I�@.{@'�@!hs@p�@&�@V@��@"�@�P@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��A��A��A��A��A��A��A��mA��;A��/A���AƝ�AƏ\A�VA�A�A�=qA�9XA�-A�"�A�oA�A���A��A��A��`A���AŴ9A�VA��A�1A�hsA��A�\)A�&�A�
=A��mA��^A��PA�9XA�JA�p�A���A�-A��A�t�A�bA�ƨA�VA�  A��PA�dZA�&�A��A���A�x�A�K�A���A�\)A���A��A�ȴA��A���A��A���A��A��A�+A��A�oA�t�A�&�A���A���A�n�A�VA���A�x�A��A���A��A��#A���A��FA�p�A���A�;dA���A�|�A���A��A��/A���A��A��A�  A�\)A�t�A��HA��uA�hsA���A��A�VA���A�  A�M�A�ZA�1'A��wA�ZA�&�A���A�t�A�ȴA���A�?}A��PA�p�A�{A���A��FA�  A�oA���A�|�A�v�A�VA�XA���A�;dA��A���A�ZA�+A}�wAz~�AxAuAs�^Ao�AmdZAi+Ac+A]\)AZM�AV�HAU�AQ��AM&�AJffAH��AF1'AD5?AB{A@r�A?\)A<�A9hsA8n�A7oA5��A4��A3�mA3`BA2��A1�wA0ffA/��A.��A-p�A,=qA+&�A*5?A'�A&9XA%�A$�A$bA"�RA!�A bNA��A�A�A��A;dA��A�AZA�A�FA�RA��AM�A�-AG�A��Ax�A
=A�FA��A(�A�hA"�AƨA%A�jAbA�wA�A
Q�An�A�/AƨA��An�Al�Av�AƨAO�A ��A J@�5?@�1@�J@�C�@��@�
=@�@�V@��@��@�$�@��@��@�O�@�P@�~�@�x�@䛦@�(�@�\)@�@�X@ޗ�@���@���@ݩ�@ݑh@݁@�O�@��`@�1@�l�@��@ו�@�{@Ձ@ԣ�@�1'@�C�@җ�@�v�@�V@�=q@�@�V@�l�@��@��T@�z�@��@�;d@�^5@�?}@��`@���@ț�@�z�@�Z@�1'@ǶF@Ɵ�@�=q@�@��@���@�%@�v�@��@���@���@�z�@�I�@�A�@�Q�@�j@���@���@��D@��@�A�@�ƨ@�\)@��@�@���@��7@�/@��D@�b@���@�t�@�V@��R@��@��w@���@�O�@� �@��@��@��@�"�@�ff@�?}@��`@��@�K�@��@�$�@��@�5?@�hs@�@���@�9X@�\)@�S�@��y@�E�@��+@�`B@�C�@�$�@�7L@���@�J@���@��@�K�@��\@�n�@�^5@��@���@��-@���@��h@��@��h@�Ĝ@��@�j@�9X@�b@�(�@�(�@� �@�1@��;@��@���@��@�|�@�|�@�|�@�t�@�t�@�t�@�t�@�t�@�dZ@�+@���@��\@�V@���@�G�@��u@�9X@�1@��w@�|�@��@��@��^@�%@��u@�b@��
@���@�S�@�o@���@���@�$�@��@��9@��j@��`@���@���@���@�&�@�/@�Ĝ@��P@���@���@���@�&�@�?}@���@�A�@��@��y@��\@�-@��T@��-@�x�@��@���@�ƨ@�"�@��@��^@��h@��^@��T@���@�x�@��@���@�Ĝ@�r�@�9X@���@��P@�\)@�"�@�ȴ@���@��\@�n�@�M�@�=q@�$�@��@��@�V@��j@��@��@���@��u@��@�Q�@��@���@���@���@���@��@�o@��+@�=q@�{@���@��@��@�@���@��7@�&�@���@��D@�z�@�j@�j@�33@~ff@st�@f��@^ff@Vff@Pr�@K33@B�@;@4I�@.{@'�@!hs@p�@&�@V@��@"�@�P@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
�B
�B
�B
�B
�B
�B
�yB
�yB
�yB
�yB
�sB
�sB
�sB
�mB
�mB
�mB
�mB
�fB
�fB
�fB
�`B
�`B
�`B
�`B
�ZB
�NB
�BB
�B
ɺB
�dB
�9B
�-B
�^B
ɺB
�5B%BhB�B"�B �B�B�B�B"�B9XBH�BO�B[#B�B��B��B��B�B��B��B�B�B1B�B6FBI�BP�B[#B~�B��B��B�XB�B��B��B��B��B��B��B��B��B�B�^B�jBB��B��B��B��B��B��B�XB�-B�B��B��B�!B�3B�3B�'B�B��B�oB�1B`BB$�BDB<jBp�By�B\)BL�BR�B.B&�B$�B�B%B�B�qB��By�B=qB.B,B,BbB
�B
�
B
��B
��B
ĜB
�FB
��B
�VB
p�B
aHB
I�B
7LB
!�B
B	�TB	��B	�)B	��B	�?B	��B	s�B	A�B	)�B	�B	bB	B�B�TB�B��BɺB�wB�9B�FB�B��B��B�B��B�B�B�3B�'B�3B�B��B��B��B��B��B��B��B��B�=B�1B�=B�B�bB~�B~�B�B|�Bz�B}�B{�Bx�By�Bt�Bt�Bv�Bu�Bt�By�Bt�Bt�Bt�Bq�Bo�BjBhsBiyBhsBhsBhsBcTBbNBcTBaHB`BBaHBaHBYBYBYBS�BQ�BT�B]/BVBQ�BXB\)BW
BZB^5BQ�BR�BT�BR�BR�BP�BQ�BXBYBS�BJ�BO�BP�BK�BN�BR�BT�BQ�B\)B`BBbNBcTBdZBdZBdZBe`BgmBhsBl�Bl�Bp�Bo�Bo�Bo�Bo�Bp�Bp�Bq�Br�Bt�Bx�Bw�Bx�Bx�Bw�Bx�B{�B� B�B�1B�7B�DB�JB�PB�bB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�!B�FB�}B��BBĜBƨBȴB��B��B��B��B��B��B��B��B��BȴBBB�qB�jB�XB�LB�XB�}BĜBƨBĜBŢB��B�5B�TB��B	oB	�B	2-B	33B	'�B	#�B	$�B	"�B	#�B	"�B	!�B	(�B	+B	 �B	�B	�B	!�B	�B	oB	{B	hB	bB	oB	�B	�B	�B	!�B	%�B	'�B	+B	1'B	5?B	7LB	8RB	:^B	?}B	G�B	I�B	K�B	N�B	P�B	Q�B	Q�B	Q�B	R�B	R�B	R�B	R�B	R�B	R�B	R�B	R�B	R�B	T�B	[#B	^5B	_;B	`BB	aHB	bNB	cTB	iyB	jB	k�B	r�B	p�B	u�B	x�B	y�B	z�B	z�B	z�B	z�B	{�B	|�B	|�B	|�B	~�B	�B	�B	�B	�1B	�7B	�DB	�PB	�uB	��B	��B	��B	��B	�B	�-B	�RB	�^B	�qB	�XB	�XB	�LB	�RB	�^B	�dB	��B	�qB	�wB	��B	�}B	�qB	B	ŢB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�#B	�#B	�#B	�)B	�)B	�)B	�)B	�)B	�/B	�;B	�HB	�NB	�HB	�NB	�TB	�TB	�`B	�fB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B
+B
bB
�B
#�B
+B
/B
9XB
?}B
G�B
M�B
T�B
[#B
_;B
iyB
m�B
q�B
u�B
v�B
y�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
�B
�B
�B
�B
�B
�B
�yB
�yB
�yB
�yB
�yB
�yB
�yB
�sB
�mB
�mB
�mB
�fB
�fB
�fB
�`B
�`B
�`B
�`B
�ZB
�NB
�HB
�)B
��B
�wB
�LB
�3B
�dB
��B
�;B+BoB �B#�B$�B�B�B�B$�B;dBJ�BQ�BaHB�%B��B��B��B�!B��B��B�B�BDB�B:^BK�BQ�B[#B~�B��B�B�dB�'B��B��B��B��B��B��B��B��B�'B�jB�qBB��B��B��B��B��BŢB�dB�9B�3B�B�B�'B�?B�RB�?B�!B��B��B�hBm�B-B+B8RBt�B�B`BBQ�BXB0!B(�B)�B�BJB�NBƨB��B�BB�B0!B/B6FB�B
�B
�B
��B
��B
��B
��B
�'B
��B
u�B
l�B
T�B
>wB
)�B
PB	�sB	�
B	�BB	�)B	�^B	��B	� B	M�B	1'B	#�B	{B	DB��B�B�5B�)B��BŢB�XB�^B�9B�3B�B�!B�B�B�B�?B�3B�FB�-B��B��B��B��B��B��B��B��B�JB�=B�PB�1B��B�B�B�%B}�B{�B� B}�B}�B|�Bu�Bv�By�By�Bx�B{�Bv�By�Bu�Bs�Bt�Bm�Bk�Bk�BjBm�Bk�BdZBdZBdZBdZBcTBhsBgmB]/B\)B[#BXBT�BW
B^5BXBS�BZB_;BZB^5BbNBR�BS�BVBS�BT�BT�BT�BZB\)BW
BL�BQ�BR�BL�BP�BT�BW
BVB]/BaHBcTBcTBdZBe`Be`BgmBhsBk�Bp�Bo�Bq�Bq�Bp�Bq�Bp�Bq�Bq�Br�Bs�Bv�B{�Bx�Bz�B{�Bx�Bz�B}�B�B�%B�1B�=B�JB�PB�VB�bB��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�!B�FB�}B��BÖBŢBǮBɺB��B��B��B��B��B��B��B��B��B��BŢBĜB�}B�wB�dB�XB�^B�}BĜBǮBƨBƨBɺB�/B�/B��B	\B	�B	49B	9XB	)�B	%�B	&�B	"�B	$�B	#�B	!�B	(�B	/B	"�B	�B	�B	#�B	�B	uB	�B	oB	bB	oB	�B	�B	�B	!�B	%�B	'�B	+B	2-B	5?B	7LB	9XB	:^B	?}B	G�B	I�B	K�B	O�B	Q�B	Q�B	Q�B	Q�B	R�B	R�B	R�B	R�B	R�B	R�B	R�B	R�B	S�B	VB	\)B	_;B	`BB	aHB	bNB	cTB	dZB	jB	k�B	l�B	t�B	q�B	v�B	y�B	z�B	{�B	{�B	{�B	{�B	{�B	}�B	}�B	~�B	� B	�B	�B	�B	�1B	�7B	�DB	�PB	�{B	��B	��B	��B	��B	�B	�-B	�XB	�dB	�wB	�^B	�^B	�RB	�XB	�^B	�jB	��B	�wB	�}B	��B	��B	�wB	B	ŢB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�#B	�#B	�#B	�)B	�)B	�)B	�/B	�/B	�5B	�BB	�HB	�NB	�HB	�NB	�TB	�ZB	�fB	�fB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B
+B
bB
�B
#�B
+B
/B
9XB
?}B
G�B
M�B
T�B
[#B
`BB
iyB
m�B
q�B
u�B
v�B
z�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<u<49X<D��<#�
<#�
<49X<49X<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.3 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201101452042012011014520420120110145204  AO  ARGQ                                                                        20111130143831  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130143831  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120110145204  IP                  G�O�G�O�G�O�                