CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-28T23:01:50Z UW 3.1 conversion   
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901072 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               &A   AO  20111130143857  20190522121828  1728_5048_038                   2C  D   APEX                            2142                            040306                          846 @ԉC�4W�1   @ԉD:� @5�hr� ��c6ȴ9X1   GPS     Primary sampling: mixed [deeper than nominal 990dbar: discrete; nominal 990dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B ffB(ffB/��B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CO�fCR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� DfD�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D#��D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DGfDG� DH  DH� DI  DI� DJ  DJ� DJ��DKy�DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dvs3DyL�D���D�#3D��D�� D��D�FfD�s3D��fD���D��D�ffDǼ�D��fD� Dڌ�D�fD��fD��D�Y�D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @s33@�ff@�ffA33A;33A[33A{33A���A���A���A���A͙�Aݙ�A홚A���B��B��B��B33B'33B.ffB6��B>��BF��BN��BV��B^��Bf��Bn��Bv��B~��B�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffC�3C�3C�3C�3C	�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C!�3C#�3C%�3C'�3C)�3C+�3C-�3C/�3C1�3C3�3C5�3C7�3C9�3C;�3C=�3C?�3CA�3CC�3CE�3CG�3CI�3CK�3CM�3CO��CQ�3CS�3CU�3CW�3CY�3C[�3C]�3C_�3Ca�3Cc�3Ce�3Cg�3Ci�3Ck�3Cm�3Co�3Cq�3Cs�3Cu�3Cw�3Cy�3C{�3C}�3C�3C�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC��fC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC��fC�ٚC�ٚC��fC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚD l�D ��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��D	l�D	��D
l�D
�3Ds3D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��D l�D ��D!l�D!��D"l�D"��D#l�D#�fD$l�D$��D%l�D%��D&l�D&��D'l�D'��D(l�D(��D)l�D)��D*l�D*��D+l�D+��D,l�D,��D-l�D-��D.l�D.��D/l�D/��D0l�D0��D1l�D1��D2l�D2��D3l�D3��D4l�D4��D5l�D5��D6l�D6��D7l�D7��D8l�D8��D9l�D9��D:l�D:��D;l�D;��D<l�D<��D=l�D=��D>l�D>��D?l�D?��D@l�D@��DAl�DA��DBl�DB��DCl�DC��DDl�DD��DEl�DE��DFl�DF�3DGl�DG��DHl�DH��DIl�DI��DJl�DJ�fDKffDK��DLl�DL��DMl�DM��DNl�DN��DOl�DO��DPl�DP��DQl�DQ��DRl�DR��DSl�DS��DTl�DT��DUl�DU��DVl�DV��DWl�DW��DXl�DX��DYl�DY��DZl�DZ��D[l�D[��D\l�D\��D]l�D]��D^l�D^��D_l�D_��D`l�D`��Dal�Da��Dbl�Db��Dcl�Dc��Ddl�Dd��Del�De��Dfl�Df��Dgl�Dg��Dhl�Dh��Dil�Di��Djl�Dj��Dkl�Dk��Dll�Dl��Dml�Dm��Dnl�Dn��Dol�Do��Dpl�Dp��Dql�Dq��Drl�Dr��Dsl�Ds��Dtl�Dt��Dul�Du��Dv` Dy9�D�� D��D�3D��fD�3D�<�D�i�D���D��3D� D�\�Dǳ3D���D�fDڃ3D��D���D� D�P D��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��yA���AΑhA�7LA��`Aͺ^AͰ!Aͣ�A͝�A͗�A͍PA͇+A�~�A�p�A�M�A̺^A�A�ĜA�x�A�\)A�$�A���A�A�5?A��RA�(�A�ȴA���A��jA�ĜA��A�%A�/A�l�A�JA���A��A��
A��A�E�A�z�A� �A��yA��mA�hsA�(�A��A�=qA��hA�%A���A���A�+A��\A�hsA��`A�$�A�XA��FA�=qA��HA���A�l�A�ZA�A��^A�=qA�ZA���A�n�A�%A���A�M�A���A�$�A�"�A�5?A���A��
A�r�A���A�VA�A�A���A�p�A���A�^5A��A�jA�C�A���A��A�VA�-A��wA�=qA�^5A���A�ƨA��FA�1'A�n�A�JA���A���A�`BA�
=A��A�l�A�=qA�hsA�hsA���A~^5A}
=A|1'AvQ�Ar-Ao�
AnM�Am/Ak�mAj{Ai7LAgO�AcK�A`�uA_S�A^  A\ZA[+AY�AVĜAUC�ATn�AS"�AP�!AL�/AI�AHr�AG��AGK�AF�`AFn�AC��AB(�AAC�A?�TA?33A>�A>��A>  A=dZA=/A<��A;�;A;%A:JA8��A6v�A5�A4ĜA2�uA1
=A/��A.��A.bNA-�A,�/A+�A*�!A)�A)p�A(�RA'hsA&�uA%ƨA$��A#
=A"5?A!&�A�;A�HA�AdZA33A��AĜAp�AA��AƨA"�A��A(�A�
A��AG�A�AG�A=qA\)AbNAp�Az�AC�A=qAG�A
ĜA
1A~�AA?}A�DA5?A��A&�A�!A5?A��A&�A~�A?}@���@��@�x�@��@��+@���@��H@�bN@�h@�v�@�?}@�1'@�@噚@㕁@◍@��@߶F@ޗ�@� �@ڇ+@ى7@ؓu@�|�@�=q@���@�5?@�I�@υ@�"�@��H@�@�(�@�/@�9X@ǶF@��y@�5?@ř�@ēu@���@�-@���@�Ĝ@�o@��h@�(�@�\)@�
=@��R@�-@�x�@��@�ƨ@�33@���@���@�^5@��@�V@�r�@��@���@�M�@���@�x�@�Ĝ@���@���@�n�@�{@��^@�/@��@�Q�@��@���@��@�o@�ȴ@�^5@��^@���@��/@���@�bN@��
@���@�+@��!@��@��#@�X@�G�@�`B@�p�@�X@�V@���@��/@�Ĝ@���@�r�@��@�+@���@�^5@�n�@���@���@��T@���@�1@���@�"�@�ȴ@�@���@�x�@�p�@�`B@��`@��j@��u@��@�z�@�Z@��@� �@�(�@�1@��F@��@�t�@�+@��y@���@�^5@��@�`B@���@�A�@��@�|�@���@���@�~�@�V@�n�@�ȴ@�;d@�I�@�7L@���@��@���@�X@�%@���@���@���@�bN@��
@���@��;@�Q�@�(�@��F@��!@�r�@���@�  @�C�@��@�V@�hs@�X@�$�@�ff@�ff@��#@�p�@�?}@�O�@�x�@��7@�p�@��@�Ĝ@�j@��@���@�1@��
@�S�@���@�ff@�@��@�5?@�M�@�V@�M�@�$�@�@��@��@��@��@��#@��-@��7@�?}@��/@��D@��@�+@��@���@��-@�hs@�7L@��j@�(�@��m@�\)@�v�@��T@��7@��@��@�I�@�b@���@���@���@�I�@�I�@��w@�t�@���@��+@�ff@�J@���@��-@���@�x�@�`B@�O�@�G�@�/@��@��/@���@�Z@�9X@�@\)@~�@~V@}�@|�@|�@{��@{S�@z�\@y��@yX@w�;@p�`@h��@_K�@Y�#@So@L��@D��@;@6{@.��@*J@$�D@ ��@�@�`@��@��@�@
�!@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��yA���AΑhA�7LA��`Aͺ^AͰ!Aͣ�A͝�A͗�A͍PA͇+A�~�A�p�A�M�A̺^A�A�ĜA�x�A�\)A�$�A���A�A�5?A��RA�(�A�ȴA���A��jA�ĜA��A�%A�/A�l�A�JA���A��A��
A��A�E�A�z�A� �A��yA��mA�hsA�(�A��A�=qA��hA�%A���A���A�+A��\A�hsA��`A�$�A�XA��FA�=qA��HA���A�l�A�ZA�A��^A�=qA�ZA���A�n�A�%A���A�M�A���A�$�A�"�A�5?A���A��
A�r�A���A�VA�A�A���A�p�A���A�^5A��A�jA�C�A���A��A�VA�-A��wA�=qA�^5A���A�ƨA��FA�1'A�n�A�JA���A���A�`BA�
=A��A�l�A�=qA�hsA�hsA���A~^5A}
=A|1'AvQ�Ar-Ao�
AnM�Am/Ak�mAj{Ai7LAgO�AcK�A`�uA_S�A^  A\ZA[+AY�AVĜAUC�ATn�AS"�AP�!AL�/AI�AHr�AG��AGK�AF�`AFn�AC��AB(�AAC�A?�TA?33A>�A>��A>  A=dZA=/A<��A;�;A;%A:JA8��A6v�A5�A4ĜA2�uA1
=A/��A.��A.bNA-�A,�/A+�A*�!A)�A)p�A(�RA'hsA&�uA%ƨA$��A#
=A"5?A!&�A�;A�HA�AdZA33A��AĜAp�AA��AƨA"�A��A(�A�
A��AG�A�AG�A=qA\)AbNAp�Az�AC�A=qAG�A
ĜA
1A~�AA?}A�DA5?A��A&�A�!A5?A��A&�A~�A?}@���@��@�x�@��@��+@���@��H@�bN@�h@�v�@�?}@�1'@�@噚@㕁@◍@��@߶F@ޗ�@� �@ڇ+@ى7@ؓu@�|�@�=q@���@�5?@�I�@υ@�"�@��H@�@�(�@�/@�9X@ǶF@��y@�5?@ř�@ēu@���@�-@���@�Ĝ@�o@��h@�(�@�\)@�
=@��R@�-@�x�@��@�ƨ@�33@���@���@�^5@��@�V@�r�@��@���@�M�@���@�x�@�Ĝ@���@���@�n�@�{@��^@�/@��@�Q�@��@���@��@�o@�ȴ@�^5@��^@���@��/@���@�bN@��
@���@�+@��!@��@��#@�X@�G�@�`B@�p�@�X@�V@���@��/@�Ĝ@���@�r�@��@�+@���@�^5@�n�@���@���@��T@���@�1@���@�"�@�ȴ@�@���@�x�@�p�@�`B@��`@��j@��u@��@�z�@�Z@��@� �@�(�@�1@��F@��@�t�@�+@��y@���@�^5@��@�`B@���@�A�@��@�|�@���@���@�~�@�V@�n�@�ȴ@�;d@�I�@�7L@���@��@���@�X@�%@���@���@���@�bN@��
@���@��;@�Q�@�(�@��F@��!@�r�@���@�  @�C�@��@�V@�hs@�X@�$�@�ff@�ff@��#@�p�@�?}@�O�@�x�@��7@�p�@��@�Ĝ@�j@��@���@�1@��
@�S�@���@�ff@�@��@�5?@�M�@�V@�M�@�$�@�@��@��@��@��@��#@��-@��7@�?}@��/@��D@��@�+@��@���@��-@�hs@�7L@��j@�(�@��m@�\)@�v�@��T@��7@��@��@�I�@�b@���@���@���@�I�@�I�@��w@�t�@���@��+@�ff@�J@���@��-@���@�x�@�`B@�O�@�G�@�/@��@��/@���@�Z@�9X@�@\)@~�@~V@}�@|�@|�@{��@{S�@z�\@y��@yX@w�;@p�`@h��@_K�@Y�#@So@L��@D��@;@6{@.��@*J@$�D@ ��@�@�`@��@��@�@
�!@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B�
B�
B�
B�B�B�
B�
B�B�B�B�B�B�B��B��B��B�B)�BC�Br�B�+B�7B�B�^BƨB��B��B��B�wB�B�B�B��BɺB��B�B�HB�sB�B�
B�B��B  B  B��B��B��B�B��B��B��B��B��B�B�B�B�B�NB�;B�BB�mB��B�B�B�sB�)B��B��BǮBÖB�wB�^B�-B��B��B�uB�B}�Br�Bn�BdZBZB[#BR�BN�BH�BH�B5?B�B{BB�B�HB�B��B�XB��B|�Bn�B^5BH�B49B�BB
�TB
�wB
��B
�PB
� B
m�B
Q�B
%�B
oB
B	�5B	�qB	��B	��B	�uB	�1B	q�B	cTB	R�B	5?B	$�B	�B	JB	+B��B�B�ZB�HB�B��BǮB��B��B�{B�hB�bB�bB�JB�=B�DB�7B�=B�=B�=B�DB�JB�=B�=B�1B�7B�7B�7B�PB�B�B�B� Bt�BjBdZBcTBbNB]/BYBZBZB^5B]/BiyB_;BdZBgmBk�BiyBn�BhsBo�Bu�Bt�Bu�Bv�Bs�Br�Bs�Bo�Bn�Bn�Bn�Bn�Bn�Bm�Bm�Bk�Be`BbNB^5BZBVBR�BP�BN�BL�BK�BJ�BJ�BJ�BJ�BJ�BK�BL�BL�BL�BQ�BQ�BP�BN�BQ�BL�BM�BF�BF�BE�BB�B=qB8RB<jB7LB33B2-B8RB7LB2-B33B49B5?B49B33B1'B0!B1'B0!B-B)�B,B-B,B,B+B)�B+B+B-B-B.B0!B0!B2-B5?B6FB8RB;dB?}BA�BF�BH�BI�BJ�BL�BO�BT�BYB[#B]/B]/B^5B`BBe`BhsBm�Bo�Bq�Bs�Bt�Bx�B{�Bw�Bw�Bx�B� B� B�B�B�DB�PB�bB�uB�{B��B��B��B��B��B�B�B�B�-B�LB�dB��BȴB��B��B��B��B��B�B�B�#B�)B�/B�BB�yB�B�B�B��B��B��B��B	B	B	B	JB	PB	bB	hB	uB	�B	�B	�B	�B	�B	 �B	 �B	$�B	+B	.B	1'B	33B	33B	6FB	8RB	9XB	;dB	=qB	A�B	C�B	F�B	H�B	H�B	H�B	K�B	O�B	Q�B	P�B	Q�B	XB	]/B	dZB	n�B	u�B	x�B	{�B	~�B	~�B	~�B	�B	�B	�B	�B	�B	�+B	�\B	�\B	�VB	�7B	�B	�+B	�DB	�PB	�JB	�DB	�DB	�VB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�9B	�RB	�LB	�FB	�LB	�LB	�^B	�wB	��B	ÖB	ŢB	ƨB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	��B	��B	�
B	�B	�B	�
B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�#B	�/B	�5B	�BB	�NB	�NB	�HB	�ZB	�ZB	�ZB	�`B	�`B	�`B	�`B	�`B	�`B	�`B	�fB	�mB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B
bB
�B
�B
 �B
(�B
0!B
6FB
B�B
H�B
L�B
Q�B
XB
aHB
gmB
l�B
q�B
s�B
y�B
|�B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B��B��B�B�B�B�B�B�
B�
B�B�B�B�B�
B�B�B�B�#B�B,BF�Bt�B�=B�uB�3B��B��B��B�B��B��B�-B�'B�BB��B��B�B�NB�B�B�B�B��BBB��B��B��B�B��BB��B��B��B��B�B�/B�)B�ZB�HB�HB�sB��B��B�B�B�;B��B��BɺBŢB�}B�qB�?B�B��B��B�B� Bv�Br�BgmB\)B^5BT�BP�BK�BN�B9XB �B�B
=B��B�ZB�5B��BÖB��B� Br�BffBN�B:^B&�B	7B
�B
ƨB
�B
�oB
�%B
u�B
aHB
)�B
�B
�B	�B	ŢB	�B	��B	��B	�\B	u�B	k�B	bNB	?}B	)�B	!�B	oB	DB	B��B�sB�ZB�)B�B��B�'B��B��B�oB�hB�oB�oB�\B�VB�PB�JB�DB�DB�PB�VB�DB�DB�DB�JB�PB�VB�{B�%B�1B�JB�By�Bm�BffBe`BffBaHB^5B]/B\)BaHBaHBk�BaHBffBl�Bm�Bl�Br�Bk�Bq�Bw�Bu�Bv�Bw�Bw�Bw�Bv�Br�Bp�Bo�Bp�Bo�Bo�Bn�Bp�Bo�BiyBe`BbNB]/BZBW
BT�BQ�BN�BN�BO�BM�BL�BM�BL�BM�BO�BN�BN�BS�BS�BS�BS�BW
BQ�BT�BJ�BH�BG�BG�BB�B=qBA�B9XB5?B49B:^B:^B49B5?B6FB7LB8RB5?B33B2-B33B2-B/B.B/B.B-B-B-B-B0!B-B.B/B0!B1'B2-B5?B7LB7LB:^B>wBB�BD�BH�BI�BJ�BK�BN�BQ�BW
BYB[#B^5B^5B_;BbNBffBjBo�Bp�Br�Bt�Bv�Bz�B}�Bx�Bx�By�B�B�B�B�B�DB�VB�hB�{B��B��B��B��B��B��B�B�B�!B�3B�RB�jBBȴB��B��B��B��B��B�B�B�)B�/B�5B�BB�B�B�B�B��B��B��B��B	B	B	B	VB	VB	hB	hB	uB	�B	�B	�B	�B	�B	 �B	!�B	$�B	+B	.B	2-B	33B	33B	7LB	9XB	:^B	<jB	>wB	B�B	D�B	G�B	I�B	H�B	H�B	K�B	Q�B	Q�B	P�B	Q�B	W
B	\)B	cTB	n�B	u�B	x�B	|�B	� B	� B	~�B	�B	�B	�B	�B	�B	�+B	�bB	�bB	�bB	�PB	�B	�1B	�JB	�VB	�JB	�PB	�DB	�PB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�9B	�XB	�RB	�LB	�RB	�RB	�^B	�wB	��B	ÖB	ŢB	ǮB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�#B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�B	�#B	�5B	�;B	�HB	�TB	�NB	�NB	�`B	�ZB	�ZB	�`B	�`B	�`B	�`B	�`B	�`B	�fB	�mB	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B
bB
�B
�B
!�B
(�B
0!B
7LB
B�B
I�B
L�B
Q�B
XB
aHB
gmB
l�B
q�B
s�B
y�B
}�B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<e`B<#�
<#�
<�C�<T��<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.3 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201101452062012011014520620120110145206  AO  ARGQ                                                                        20111130143857  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130143857  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120110145206  IP                  G�O�G�O�G�O�                