CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-28T23:01:47Z UW 3.1 conversion   
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901072 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               A   AO  20111130143753  20190522121828  1728_5048_028                   2C  D   APEX                            2142                            040306                          846 @�po�I�1   @�ppK� 	@6Cn��P�cK�vȴ91   GPS     Primary sampling: mixed [deeper than nominal 990dbar: discrete; nominal 990dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`ffBg��Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C[�fC^  C`  Cb  Cd  Cf  Ch  Cj�Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,y�D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7fD7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Day�Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� DyffD�  D�9�D�p D�� D��D�C3D�s3D���D�� D�#3D�` Dǹ�D�� D��D�vfD�3D��D�fD�FfD�y�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @s33@�ff@�ffA33A;33A[33A{33A���A���A���A���A͙�Aݙ�A홚A���B��B��B��B��B&��B.��B6��B>��BF��BN��BV��B_33BfffBn��Bv��B~��B�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffC�3C�3C�3C�3C	�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C!�3C#�3C%�3C'�3C)�3C+�3C-�3C/�3C1�3C3�3C5�3C7�3C9�3C;�3C=�3C?�3CA�3CC�3CE�3CG�3CI�3CK�3CM�3CO�3CQ�3CS�3CU�3CW�3CY�3C[��C]�3C_�3Ca�3Cc�3Ce�3Cg�3Ci��Ck�3Cm�3Co�3Cq�3Cs�3Cu�3Cw�3Cy�3C{�3C}�3C�3C��fC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC��fC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚD l�D ��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��D	l�D	��D
l�D
��Dl�D��Dl�D��DffD��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��D l�D ��D!l�D!��D"l�D"��D#l�D#��D$l�D$��D%l�D%��D&l�D&��D'l�D'��D(l�D(��D)l�D)��D*l�D*��D+l�D+��D,ffD,��D-l�D-��D.l�D.��D/l�D/��D0l�D0��D1l�D1��D2l�D2��D3l�D3��D4l�D4��D5l�D5��D6l�D6�3D7l�D7��D8l�D8��D9l�D9��D:l�D:��D;l�D;��D<l�D<��D=l�D=��D>l�D>��D?l�D?��D@l�D@��DAl�DA��DBl�DB��DCl�DC��DDl�DD��DEl�DE��DFl�DF��DGl�DG��DHl�DH��DIl�DI��DJl�DJ��DKl�DK��DLl�DL��DMl�DM��DNl�DN��DOl�DO��DPl�DP��DQl�DQ��DRl�DR��DSl�DS��DTl�DT��DUl�DU��DVl�DV��DWl�DW��DXl�DX��DYl�DY��DZl�DZ��D[l�D[��D\l�D\��D]l�D]��D^l�D^��D_l�D_��D`l�D`��DaffDa��Dbl�Db��Dcl�Dc��Ddl�Dd��Del�De��Dfl�Df��Dgl�Dg��Dhl�Dh��Dil�Di��Djl�Dj��Dkl�Dk��Dll�Dl��Dml�Dm��Dnl�Dn��Dol�Do��Dpl�Dp��Dql�Dq��Drl�Dr��Dsl�Ds��Dtl�Dt��Dul�Du��Dvl�DyS3D��fD�0 D�ffD��fD�3D�9�D�i�D�� D��fD��D�VfDǰ D��fD�3D�l�D���D�� D��D�<�D�p 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��RA��^A�33A��HA��FA��7A�p�A�VA�C�A�9XA�1'A�-A�$�A��A�
=A�1A�%A�%A�A�A�A�A�A�A�  A���A��A��A���A�&�A��A���A��A���A�C�A��yA�A�K�A�$�A��`A�A���A�/A�ƨA���A��7A�VA��A��wA�ZA��A��A�
=A��A�oA��;A���A�oA��A�ffA���A��
A���A�5?A�oA���A�9XA�ȴA��9A�A�A�ȴA��A�-A��A�S�A���A��A��FA��PA��A��A�;dA��
A��RA��;A�$�A���A�9XA�l�A���A��A�ZA��yA��\A��A���A�?}A� �A�A��A��A��A�(�A�=qA��uA�I�A��A�A�M�A��/A�\)A{hsAy;dAwS�Avz�Au��Au%As�Aq��Ap�DAo|�AoVAnffAn �Amp�Aj�Ag�
Af�yAf��AfffAf�Ac7LA`�+A_hsA^��A]ƨA]S�AZĜAV��AV  AR�/AO��AM�;ALn�AK�FAI��AGAF-ADQ�ABA�AAK�A@n�A>�`A<(�A:��A9��A8z�A6�A5�TA4ZA3�^A3`BA2�+A1��A01A.��A.jA-�TA,ȴA+�A*�HA*E�A)��A)
=A(  A'K�A&�A&�!A&^5A%x�A$��A#�A"�A!"�A!%A �yA �jA ^5A�A^5AXAZA;dA(�A��AVA�mA�A�FA�/A��AƨA  AhsA�A��A$�A��A%A
JA	��A	dZA	A��AA�A��AoA��A�TA��A��Ax�AO�A7LA�A �HA ~�@���@�o@�@� �@��H@��F@�?}@�hs@�V@�!@��`@�l�@�+@�\@�M�@��#@�x�@��`@�Ĝ@�=q@�
=@�=q@܃@۾w@�"�@�5?@׾w@�ff@�hs@ԣ�@Ӿw@��@�n�@�M�@�E�@���@�/@�@�V@�5?@���@͑h@�@��T@͡�@���@�/@Ӆ@�1@�\)@�C�@��@ҟ�@�~�@�7L@�|�@�-@ͩ�@�V@�ƨ@�C�@�~�@�@�S�@��T@��7@��H@��@�ƨ@��R@�"�@�(�@�l�@�^5@�/@��F@�;d@���@��@�n�@���@�%@�V@��/@��u@��D@�|�@�n�@��+@�ȴ@���@�(�@���@�bN@���@��@��@�@��\@�dZ@���@��w@�\)@�@�Z@�@��@��-@���@��7@�`B@�hs@�@�n�@��@���@�1@�j@�bN@��!@�G�@���@�%@��h@�M�@���@��@��y@��@��@��
@��P@���@���@��T@�G�@�@��@�V@���@���@�V@�Z@��F@��@� �@�V@��@�V@�r�@�K�@�v�@�p�@��u@�^5@���@��@��@� �@�A�@�$�@���@��H@�X@��@�&�@��;@��@��@�ȴ@���@�E�@�@�@��#@��-@���@���@�`B@�/@�V@�/@�`B@��@���@���@�Ĝ@��`@��9@�(�@��@�1@���@�G�@��7@���@�x�@���@��@���@���@���@��@�bN@��@���@�o@���@��+@��+@�~�@�^5@�E�@�E�@�~�@�K�@�bN@���@��j@�Q�@���@���@��#@�G�@��@���@�j@��u@��@�X@��@�O�@��@���@�Ĝ@��9@�Q�@��@��
@�1@��@�b@� �@�9X@��@���@��#@�M�@�V@��^@�1@�ƨ@�1'@�r�@��`@��9@��u@�j@���@�9X@��H@���@��P@�ƨ@��@�hs@���@y�@r�@g�@`��@ZM�@Qx�@K@F$�@?;d@7+@2�@,��@'K�@#"�@�T@��@ �@t�@�+111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��RA��^A�33A��HA��FA��7A�p�A�VA�C�A�9XA�1'A�-A�$�A��A�
=A�1A�%A�%A�A�A�A�A�A�A�  A���A��A��A���A�&�A��A���A��A���A�C�A��yA�A�K�A�$�A��`A�A���A�/A�ƨA���A��7A�VA��A��wA�ZA��A��A�
=A��A�oA��;A���A�oA��A�ffA���A��
A���A�5?A�oA���A�9XA�ȴA��9A�A�A�ȴA��A�-A��A�S�A���A��A��FA��PA��A��A�;dA��
A��RA��;A�$�A���A�9XA�l�A���A��A�ZA��yA��\A��A���A�?}A� �A�A��A��A��A�(�A�=qA��uA�I�A��A�A�M�A��/A�\)A{hsAy;dAwS�Avz�Au��Au%As�Aq��Ap�DAo|�AoVAnffAn �Amp�Aj�Ag�
Af�yAf��AfffAf�Ac7LA`�+A_hsA^��A]ƨA]S�AZĜAV��AV  AR�/AO��AM�;ALn�AK�FAI��AGAF-ADQ�ABA�AAK�A@n�A>�`A<(�A:��A9��A8z�A6�A5�TA4ZA3�^A3`BA2�+A1��A01A.��A.jA-�TA,ȴA+�A*�HA*E�A)��A)
=A(  A'K�A&�A&�!A&^5A%x�A$��A#�A"�A!"�A!%A �yA �jA ^5A�A^5AXAZA;dA(�A��AVA�mA�A�FA�/A��AƨA  AhsA�A��A$�A��A%A
JA	��A	dZA	A��AA�A��AoA��A�TA��A��Ax�AO�A7LA�A �HA ~�@���@�o@�@� �@��H@��F@�?}@�hs@�V@�!@��`@�l�@�+@�\@�M�@��#@�x�@��`@�Ĝ@�=q@�
=@�=q@܃@۾w@�"�@�5?@׾w@�ff@�hs@ԣ�@Ӿw@��@�n�@�M�@�E�@���@�/@�@�V@�5?@���@͑h@�@��T@͡�@���@�/@Ӆ@�1@�\)@�C�@��@ҟ�@�~�@�7L@�|�@�-@ͩ�@�V@�ƨ@�C�@�~�@�@�S�@��T@��7@��H@��@�ƨ@��R@�"�@�(�@�l�@�^5@�/@��F@�;d@���@��@�n�@���@�%@�V@��/@��u@��D@�|�@�n�@��+@�ȴ@���@�(�@���@�bN@���@��@��@�@��\@�dZ@���@��w@�\)@�@�Z@�@��@��-@���@��7@�`B@�hs@�@�n�@��@���@�1@�j@�bN@��!@�G�@���@�%@��h@�M�@���@��@��y@��@��@��
@��P@���@���@��T@�G�@�@��@�V@���@���@�V@�Z@��F@��@� �@�V@��@�V@�r�@�K�@�v�@�p�@��u@�^5@���@��@��@� �@�A�@�$�@���@��H@�X@��@�&�@��;@��@��@�ȴ@���@�E�@�@�@��#@��-@���@���@�`B@�/@�V@�/@�`B@��@���@���@�Ĝ@��`@��9@�(�@��@�1@���@�G�@��7@���@�x�@���@��@���@���@���@��@�bN@��@���@�o@���@��+@��+@�~�@�^5@�E�@�E�@�~�@�K�@�bN@���@��j@�Q�@���@���@��#@�G�@��@���@�j@��u@��@�X@��@�O�@��@���@�Ĝ@��9@�Q�@��@��
@�1@��@�b@� �@�9X@��@���@��#@�M�@�V@��^@�1@�ƨ@�1'@�r�@��`@��9@��u@�j@���@�9X@��H@���@��P@�ƨ@��@�hs@���@y�@r�@g�@`��@ZM�@Qx�@K@F$�@?;d@7+@2�@,��@'K�@#"�@�T@��@ �@t�@�+111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB"�B!�BI�B_;BffBl�Bo�Bq�Bs�Bt�Bu�Bv�Bx�Bz�B|�B}�B~�B~�B~�B~�B~�B~�B~�B� B�B�B�DB�oB�VB��B��B�uB��B��B�B��B�'B��B�B�#B�B�B�B�;B�TB�mB�fB�BB�B��BĜBɺB��B�B�B�/B�BB�5B�)B�/B�NB�sB�yB�;B�fB�;BƨBBB�wB�LB�B��B}�BdZB^5BYBaHB`BBT�BO�BI�BM�BJ�B>wB+B�B�B�mB�B��B�uB{�BbNBT�BG�B?}B(�B�B  B
�B
�B
�BB
��B
�'B
�\B
x�B
cTB
]/B
Q�B
1'B
uB	��B	�B	�fB	�NB	�#B	��B	ÖB	�^B	�!B	�B	��B	��B	��B	��B	�B	y�B	v�B	t�B	o�B	^5B	M�B	G�B	?}B	7LB	5?B	�B	VB		7B��B��B�B�BB�#B��BĜB�wB�LB�B�B��B��B��B��B��B��B�oB�\B�oB�uB�uB�{B�{B�{B�uB�{B�hB�bB�VB�=B�PB�DB�JB�VB�\B�\B�PB�DB�JB�=B�B�B�B�B�B� B� B�Bz�Bw�Bu�Bu�Bq�Bs�Bs�Bq�Bp�Bn�BjBdZBcTBbNB]/B^5B`BBcTBbNBffBe`BffBgmBdZBdZBgmBjBiyBffBdZBbNBbNBaHBaHBaHBaHBdZBhsBaHB`BB`BBgmBhsBq�Bl�BVBG�BF�BD�BE�BB�B2-B.B-B-B.B/B:^BD�BL�BO�BZB]/B_;B_;B^5B^5B^5B]/B]/B]/B]/B]/B]/B^5B`BB^5B`BBdZBm�Br�Bu�By�B�DB��B�RBĜBƨB��B��B��B��B��B��BȴBǮB��B��B��B��B�/B��B�dB�XB�qB�dB�dB�3B�wBȴBȴBɺBŢBÖBÖBĜBŢB��B��BȴBɺB��B��B��B�B�B�B�#B�fB�B�B�B�B�B�B�B��B	1B	bB	uB	oB	\B	bB	�B	oB	{B	{B	�B	�B	�B	�B	"�B	-B	8RB	8RB	A�B	D�B	H�B	?}B	:^B	:^B	?}B	I�B	O�B	R�B	XB	YB	^5B	aHB	dZB	aHB	[#B	_;B	cTB	iyB	y�B	|�B	{�B	{�B	x�B	v�B	w�B	y�B	�B	~�B	y�B	}�B	~�B	}�B	|�B	z�B	}�B	s�B	q�B	p�B	l�B	iyB	l�B	{�B	w�B	p�B	jB	cTB	[#B	XB	W
B	XB	\)B	[#B	\)B	[#B	[#B	`BB	cTB	dZB	ffB	hsB	iyB	hsB	jB	m�B	r�B	n�B	m�B	m�B	t�B	x�B	z�B	y�B	y�B	�B	�7B	�JB	�\B	�hB	�\B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�9B	�jB	�wB	��B	��B	�jB	�XB	�?B	�?B	�FB	�FB	�dB	��B	ĜB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	�B	�#B	�5B	�NB	�HB	�NB	�NB	�`B	�`B	�HB	�HB	�yB	�B	�B	�yB
B
1B
�B
!�B
1'B
5?B
;dB
A�B
E�B
J�B
N�B
R�B
XB
]/B
dZB
jB
k�B
n�B
t�B
v�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B"�B$�BK�B`BBgmBm�Bp�Br�Bs�Bt�Bu�Bv�Bx�Bz�B|�B}�B~�B~�B~�B~�B~�B~�B~�B� B�B�B�PB��B�oB��B��B�{B��B��B�B��B�!B��B�#B�)B�B�#B�)B�BB�ZB�sB�mB�NB�)B��BĜBɺB��B�B�#B�;B�TB�;B�5B�;B�TB�yB�B�BB�yB�fBȴBÖBĜB��B�jB�B��B�+BgmBbNB[#BbNBcTBW
BP�BK�BN�BN�BE�B0!B�B��B�B�NB�'B��B�BgmBYBJ�BE�B/B�BB
�B
�B
�fB
�)B
�wB
��B
� B
ffB
`BB
[#B
A�B
�B
  B	�B	�sB	�`B	�NB	�B	ǮB	�wB	�-B	�B	��B	��B	�B	��B	�B	z�B	w�B	v�B	y�B	gmB	Q�B	I�B	B�B	9XB	;dB	)�B	bB	hB	%B��B�B�NB�BB�
BȴBÖB�qB�'B�B�B��B��B��B��B��B��B�{B�{B��B��B��B��B��B��B��B��B�{B�hB�JB�\B�VB�bB�hB�bB�bB�\B�VB�VB�VB�7B�B�B�B�B�B�B�B}�Bz�Bx�Bx�Bv�Bx�Bw�Bu�Bs�Bq�Bn�Bk�BiyBdZB_;B`BBbNBe`BdZBjBffBhsBiyBffBffBjBl�Bn�Bm�BiyBe`BdZBbNBbNBbNBbNBffBjBbNBbNBcTBiyBl�Bu�Bq�B\)BK�BI�BF�BG�BH�B8RB/B.B.B.B-B9XBF�BO�BP�B[#B_;BcTBaHB`BB`BB`BB_;B^5B^5B]/B^5B_;BbNBaHB_;BaHBe`Bm�Bs�Bu�Bw�B�+B��B�RBŢBǮB��B��B��B��B��B��BɺBȴB��B��B��B�
B�NB�
B�jB�qB�}B�wB�qB�3B�qBɺB��B��BǮBĜBĜBĜBƨB��B��BȴB��B��B��B�B�B�B�B�B�`B�B�B��B��B�B�B�B��B	1B	bB	{B	�B	oB	uB	�B	uB	{B	{B	�B	�B	�B	�B	 �B	-B	8RB	8RB	A�B	G�B	J�B	@�B	:^B	9XB	>wB	H�B	O�B	R�B	XB	XB	^5B	bNB	ffB	cTB	[#B	`BB	bNB	hsB	y�B	}�B	|�B	|�B	y�B	w�B	w�B	y�B	�+B	�B	y�B	~�B	�B	� B	~�B	|�B	�B	t�B	q�B	r�B	n�B	iyB	iyB	~�B	z�B	s�B	o�B	ffB	]/B	ZB	XB	YB	]/B	\)B	]/B	[#B	[#B	`BB	cTB	dZB	gmB	hsB	iyB	hsB	jB	n�B	s�B	n�B	m�B	m�B	u�B	y�B	{�B	y�B	x�B	�B	�7B	�JB	�\B	�hB	�\B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�3B	�qB	�}B	��B	B	�qB	�XB	�FB	�FB	�LB	�FB	�^B	��B	ĜB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	��B	��B	��B	�B	�#B	�B	�B	�#B	�5B	�TB	�HB	�NB	�NB	�fB	�mB	�NB	�BB	�yB	�B	�B	�yB
B
1B
�B
!�B
1'B
5?B
;dB
A�B
E�B
J�B
N�B
R�B
XB
^5B
dZB
jB
l�B
n�B
t�B
v�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<�o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.3 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201101452022012011014520220120110145202  AO  ARGQ                                                                        20111130143753  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130143753  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120110145202  IP                  G�O�G�O�G�O�                