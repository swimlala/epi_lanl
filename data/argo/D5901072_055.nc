CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-28T23:01:54Z UW 3.1 conversion   
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901072 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               7A   AO  20111130144047  20190522121829  1728_5048_055                   2C  D   APEX                            2142                            040306                          846 @Գ~����1   @Գ�I��@5�I�^5?�cz�G�1   GPS     Primary sampling: mixed [deeper than nominal 990dbar: discrete; nominal 990dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�ff@���@���A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@ffBH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C�C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DO��DPy�DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU�fDV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dy�3D� D�)�D�i�D�� D�  D�)�D��3D���D���D��D���D�� D��3D�#3Dڀ Dਗ਼D��D� D�\�D�� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @y��@�33@�33A33A;33A[33A{33A���A���A���A���A͙�Aݙ�A홚A���B��B��B��B��B&��B.��B6��B?33BF��BN��BV��B^��Bf��Bn��Bv��B~��B�ffB�ffB���B���B�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffBB�ffB�ffB�ffB�ffC�3C�3C�3C�3C	�3C�3C�3C�3C�3C�3C�3C�3C��C�3C�3C�3C!�3C#�3C%�3C'�3C)�3C+�3C-�3C/�3C1�3C3�3C5�3C7�3C9�3C;�3C=�3C?�3CA�3CC�3CE�3CG�3CI�3CK�3CM�3CO�3CQ�3CS�3CU�3CW�3CY�3C[�3C]�3C_�3Ca�3Cc�3Ce�3Cg�3Ci�3Ck�3Cm�3Co�3Cq�3Cs�3Cu�3Cw�3Cy�3C{�3C}�3C�3C�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC��fC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC��fC�ٚC���C�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC���C���C�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚD l�D ��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��D	l�D	��D
l�D
��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��D l�D ��D!l�D!��D"l�D"��D#l�D#��D$l�D$��D%l�D%��D&l�D&��D'l�D'��D(l�D(��D)l�D)��D*l�D*��D+l�D+��D,l�D,��D-l�D-��D.l�D.��D/l�D/��D0l�D0��D1l�D1��D2l�D2��D3l�D3��D4l�D4��D5l�D5��D6l�D6��D7l�D7��D8l�D8��D9l�D9��D:l�D:��D;l�D;��D<l�D<��D=l�D=��D>l�D>��D?l�D?��D@l�D@��DAl�DA��DBl�DB��DCl�DC��DDl�DD��DEl�DE��DFl�DF��DGl�DG��DHl�DH��DIl�DI��DJl�DJ��DKl�DK��DLl�DL��DMl�DM��DNl�DN��DOl�DO�fDPffDP��DQl�DQ��DRl�DR��DSl�DS��DTl�DT��DUs3DU��DVl�DV��DWl�DW��DXl�DX��DYl�DY��DZl�DZ��D[l�D[��D\l�D\��D]l�D]��D^l�D^��D_l�D_��D`l�D`��Dal�Da��Dbl�Db��Dcl�Dc��Ddl�Dd��Del�De��Dfl�Df��Dgl�Dg��Dhl�Dh��Dil�Di��Djl�Dj��Dkl�Dk��Dll�Dl��Dml�Dm��Dnl�Dn��Dol�Do��Dpl�Dp��Dql�Dq��Drl�Dr��Dsl�Ds��Dtl�Dt��Dul�Du��Dvl�Dv��Dy� D�fD�  D�` D��fD��fD�  D���D�� D��3D�3D��3DǶfD�ٚD��D�vfD� D�� D�fD�S3D��f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�dZA�ffA�ffA�ffA�dZA�ffA�ffA�ffA�ffA�dZA�hsA�hsA�jA�jA�jA�hsA�n�A�r�A�z�A��PA���A���A���A���A���A���A���A���A���A���A���A�t�A�ZA�1A� �A���A�M�A�
=A�z�A��A��A�;dA�~�A��A��A��-A��
A�$�A�^5A�"�A�33A��yA���A���A�O�A�(�A�ZA��A� �A��^A�5?A��yA�ĜA���A�M�A���A�A���A�r�A��jA�VA���A��/A��!A�=qA��-A�"�A��^A�/A�bA�v�A�XA���A�M�A�l�A�XA���A��jA�bA�"�A�bNA���A��A�33A��A��DA�&�A���A�|�A��HA�jA~��A|�\A{7LAyoAw�wAu�At�RAtJAql�An��Am�Aj�\Afz�Ac�Ab5?A`��A^n�A\��A\ �AZ�AX^5AVĜAU��AS�AQ�-APȴAN�yAM�AMK�AL�AL�jALjAKXAI�AI��AHr�AF��AE�
AD�A@�\A=&�A;l�A9��A7��A5��A4jA2�`A1�-A0��A0�A/t�A.��A.�9A-��A,9XA+t�A)�^A)�A(�!A(9XA'�#A'?}A%��A$z�A#%A!�^A �9A (�A?}A�/AQ�A��A`BA  AffA�7A��A�7A�\A(�A��A�A�A��A��A��A\)A�A9XA�hA"�A�`AbNA�7A
�uA	ƨA	&�A�AA�PA�HAS�AbAt�AZA�hA �9@��y@�Ĝ@��@�ff@��@���@��@��@�1'@���@�v�@陚@畁@�ff@�`B@�9@�l�@�^5@��@߶F@�@��@���@��@��@�b@��@��@�r�@��m@ӶF@�o@��@�v�@�bN@�"�@ʇ+@Ɂ@�C�@ƸR@�^5@�`B@°!@���@��@���@��P@�"�@��R@�=q@��^@�?}@�V@��u@�1'@�ƨ@��@�{@��u@��;@�ƨ@��@�|�@�"�@�ȴ@�ff@��@��h@�`B@�/@��/@��@�1'@��w@���@�;d@��y@��!@�~�@�5?@���@��@�Q�@�\)@�v�@�5?@��h@�V@���@���@��P@�;d@�o@��\@��T@��@�O�@��@��`@��j@�z�@�Q�@��F@�V@�5?@��+@�-@�E�@��@�I�@�o@�
=@���@�^5@���@���@���@��7@�x�@�p�@�X@�/@�Ĝ@��u@�Q�@�\)@��H@�v�@��+@�o@�@��+@�-@���@���@��^@�V@�~�@�M�@�@��@���@���@�?}@��`@�%@��u@���@�C�@��y@���@���@���@���@��\@���@��@��H@���@�E�@�{@��T@��7@��@�r�@���@�;d@�v�@�^5@�5?@��@�$�@��@��@��@��T@��#@�@���@���@��h@�hs@�O�@�&�@��@�r�@���@���@��@�|�@��@��P@��P@�t�@�33@���@�n�@�V@�5?@�5?@��@���@���@���@�hs@�G�@�/@��@��@�A�@��@��w@��P@��w@��
@��w@�\)@���@��+@�V@��@�{@�@��@�@�{@�-@�5?@�5?@��\@�V@�E�@��T@���@�j@�bN@�Q�@�  @�S�@���@��H@�ȴ@�v�@�-@�@��@���@��^@��-@�@���@��-@�`B@���@���@� �@�ƨ@�t�@�"�@���@���@�=q@���@�7L@�V@��`@��D@�1@���@��@�S�@��@��R@�~�@�M�@�-@�$�@�J@��@���@���@��-@�`B@��@���@;d@w
=@nȴ@fff@^�y@V��@PbN@HA�@C�F@=V@6�y@0r�@*M�@%�h@!��@?}@�@t�@|�@�
@  1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�dZA�ffA�ffA�ffA�dZA�ffA�ffA�ffA�ffA�dZA�hsA�hsA�jA�jA�jA�hsA�n�A�r�A�z�A��PA���A���A���A���A���A���A���A���A���A���A���A�t�A�ZA�1A� �A���A�M�A�
=A�z�A��A��A�;dA�~�A��A��A��-A��
A�$�A�^5A�"�A�33A��yA���A���A�O�A�(�A�ZA��A� �A��^A�5?A��yA�ĜA���A�M�A���A�A���A�r�A��jA�VA���A��/A��!A�=qA��-A�"�A��^A�/A�bA�v�A�XA���A�M�A�l�A�XA���A��jA�bA�"�A�bNA���A��A�33A��A��DA�&�A���A�|�A��HA�jA~��A|�\A{7LAyoAw�wAu�At�RAtJAql�An��Am�Aj�\Afz�Ac�Ab5?A`��A^n�A\��A\ �AZ�AX^5AVĜAU��AS�AQ�-APȴAN�yAM�AMK�AL�AL�jALjAKXAI�AI��AHr�AF��AE�
AD�A@�\A=&�A;l�A9��A7��A5��A4jA2�`A1�-A0��A0�A/t�A.��A.�9A-��A,9XA+t�A)�^A)�A(�!A(9XA'�#A'?}A%��A$z�A#%A!�^A �9A (�A?}A�/AQ�A��A`BA  AffA�7A��A�7A�\A(�A��A�A�A��A��A��A\)A�A9XA�hA"�A�`AbNA�7A
�uA	ƨA	&�A�AA�PA�HAS�AbAt�AZA�hA �9@��y@�Ĝ@��@�ff@��@���@��@��@�1'@���@�v�@陚@畁@�ff@�`B@�9@�l�@�^5@��@߶F@�@��@���@��@��@�b@��@��@�r�@��m@ӶF@�o@��@�v�@�bN@�"�@ʇ+@Ɂ@�C�@ƸR@�^5@�`B@°!@���@��@���@��P@�"�@��R@�=q@��^@�?}@�V@��u@�1'@�ƨ@��@�{@��u@��;@�ƨ@��@�|�@�"�@�ȴ@�ff@��@��h@�`B@�/@��/@��@�1'@��w@���@�;d@��y@��!@�~�@�5?@���@��@�Q�@�\)@�v�@�5?@��h@�V@���@���@��P@�;d@�o@��\@��T@��@�O�@��@��`@��j@�z�@�Q�@��F@�V@�5?@��+@�-@�E�@��@�I�@�o@�
=@���@�^5@���@���@���@��7@�x�@�p�@�X@�/@�Ĝ@��u@�Q�@�\)@��H@�v�@��+@�o@�@��+@�-@���@���@��^@�V@�~�@�M�@�@��@���@���@�?}@��`@�%@��u@���@�C�@��y@���@���@���@���@��\@���@��@��H@���@�E�@�{@��T@��7@��@�r�@���@�;d@�v�@�^5@�5?@��@�$�@��@��@��@��T@��#@�@���@���@��h@�hs@�O�@�&�@��@�r�@���@���@��@�|�@��@��P@��P@�t�@�33@���@�n�@�V@�5?@�5?@��@���@���@���@�hs@�G�@�/@��@��@�A�@��@��w@��P@��w@��
@��w@�\)@���@��+@�V@��@�{@�@��@�@�{@�-@�5?@�5?@��\@�V@�E�@��T@���@�j@�bN@�Q�@�  @�S�@���@��H@�ȴ@�v�@�-@�@��@���@��^@��-@�@���@��-@�`B@���@���@� �@�ƨ@�t�@�"�@���@���@�=q@���@�7L@�V@��`@��D@�1@���@��@�S�@��@��R@�~�@�M�@�-@�$�@�J@��@���@���@��-@�`B@��@���@;d@w
=@nȴ@fff@^�y@V��@PbN@HA�@C�F@=V@6�y@0r�@*M�@%�h@!��@?}@�@t�@|�@�
@  1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�B�B�
B�
B�
B�
B�B�B�B�)B�)B�/B�)B�)B�/B�)B�5B�;B�NB�B��B��B��B��B��B��BBB	7BDBoB�B�B�BVB�B�XBz�Bx�By�B�B�\B��BƨB��B��B�B�DB�+B�{B��B��B��B�PB�Bz�Bu�Bo�Bm�BhsB`BB\)B[#BZBVBS�BS�BQ�BO�BH�BE�B=qB5?B%�B�B�B{B
=B��B�B�;B��BŢBÖB��B�Bw�BYB>wB<jB49B�BJB
�B
�/B
ĜB
��B
�bB
z�B
^5B
L�B
G�B
8RB
(�B
!�B
 �B
�B	��B	�sB	��B	ĜB	��B	�B	�DB	u�B	jB	_;B	T�B	I�B	?}B	?}B	>wB	&�B	�B		7B	B��B��B�B�B�B�B�B�mB�TB�mB�B�B�B�NB�wB��B��B��B��B�{B�DB�%B�JB�hB�hB�oB�hB�VB�1B�B� B�B�VB�7B�+B�7B�+B�%Bz�Bq�Bx�Bo�BffBhsBgmBcTB^5BdZB`BBVBXBR�BZBR�BN�BR�BM�BJ�BI�BG�BE�BE�BE�BE�BE�BD�BC�BC�BB�BB�BA�BB�BA�B@�B@�BA�B@�B>wB<jB;dB=qB<jB:^B;dB;dB7LB9XB=qB6FB1'B0!BA�B8RB,B:^B0!B+B,B1'B,B(�B)�B33B1'B.B-B1'B5?B49B49B6FB2-B2-B49B2-B/B.B-B/B.B1'B2-B2-B8RB33B2-B2-B2-B6FB8RB:^B<jB?}BE�BG�BK�BR�BYB`BBdZBhsBm�Bp�Bq�Bs�Bv�Bz�B}�B�B�B�B�B�+B�=B�=B�PB�PB�hB�uB�{B�{B��B��B��B��B��B��B��B�B�B�B�B�!B�!B�!B�-B�3B�?B�RB�jB�wB��B��BBĜBȴB��B��B�)B�/B�/B�;B�;B�BB�HB�NB�`B�B�B��B��B��B��B��B��B	  B	B	B	%B	%B		7B	hB	�B	�B	�B	�B	�B	#�B	&�B	(�B	.B	49B	5?B	7LB	:^B	=qB	?}B	D�B	H�B	H�B	G�B	F�B	I�B	K�B	K�B	L�B	O�B	S�B	\)B	^5B	bNB	dZB	e`B	gmB	iyB	n�B	p�B	r�B	v�B	w�B	x�B	y�B	z�B	{�B	|�B	}�B	�B	�B	�B	�+B	�7B	�=B	�=B	�VB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�!B	�!B	�'B	�'B	�'B	�'B	�-B	�FB	�9B	�?B	�FB	�FB	�XB	�wB	��B	�}B	�wB	�wB	�}B	��B	��B	��B	��B	B	ĜB	ƨB	ɺB	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�#B	�#B	�)B	�5B	�TB	�ZB	�`B	�ZB	�TB	�TB	�TB	�`B	�fB	�mB	�yB	�B	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B
	7B
oB
�B
'�B
-B
33B
6FB
@�B
D�B
K�B
P�B
VB
[#B
cTB
hsB
k�B
o�B
r�B
v�B
{�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�B�B�
B�
B�
B�
B�B�B�B�)B�)B�/B�)B�)B�/B�)B�5B�;B�NB�B��B��B��B��B��B��BBB	7BDBuB�B�B�BoB�B��B|�By�By�B�B�VB��BɺB��B��B�FB�\B�1B��B��B��B��B��B�+B� Bx�Bt�Bp�Bk�BbNB]/B\)B\)BYBT�BT�BS�BT�BK�BI�BC�B<jB(�B �B�B�BbB��B�B�ZB��BȴB��B�B�%B�BaHBC�B@�B8RB"�B�B
��B
�ZB
��B
�!B
��B
�B
gmB
R�B
N�B
<jB
.B
$�B
$�B
�B	��B	�B	�
B	ǮB	ǮB	�FB	�oB	y�B	n�B	e`B	YB	K�B	C�B	E�B	B�B	)�B	 �B	VB	B	B��B��B�B�B�B�B�B�ZB�B��B�B�B�BȴB�B�B��B��B��B�bB�=B�\B�{B�uB�{B�oB�hB�VB�+B�B�B�\B�=B�1B�DB�DB�7B~�Bt�B{�Bq�BiyBiyBiyBdZB`BBhsBe`BYB[#BW
B]/BT�BP�BYBO�BL�BL�BK�BI�BH�BG�BG�BG�BE�BE�BF�BE�BE�BC�BD�BC�BB�BC�BF�BD�B@�B@�B>wB@�B@�B>wB=qB=qB;dB=qB>wB8RB5?B33BE�B9XB/B<jB2-B,B.B33B.B+B-B6FB33B0!B/B33B7LB6FB7LB7LB33B33B8RB6FB33B0!B.B1'B2-B2-B33B49B=qB5?B33B33B49B7LB9XB;dB=qB@�BF�BH�BL�BS�B[#BbNBgmBiyBm�Bp�Br�Bt�Bw�B{�B~�B�B�B�B�%B�1B�DB�DB�PB�VB�oB�{B��B��B��B��B��B��B��B��B��B�B�B�B�B�'B�'B�'B�3B�9B�FB�XB�qB�}B��BBÖBƨBȴB��B�B�)B�5B�BB�HB�;B�BB�NB�TB�fB�B�B��B��B��B��B��B	  B	B	B	B	+B	%B	1B	hB	�B	�B	�B	�B	�B	"�B	&�B	)�B	/B	49B	5?B	8RB	;dB	>wB	?}B	E�B	I�B	I�B	H�B	G�B	I�B	K�B	K�B	L�B	O�B	S�B	\)B	_;B	cTB	e`B	ffB	hsB	jB	o�B	q�B	s�B	w�B	w�B	x�B	y�B	z�B	{�B	|�B	}�B	�B	�B	�B	�+B	�7B	�=B	�DB	�VB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�!B	�!B	�'B	�-B	�'B	�'B	�-B	�FB	�?B	�FB	�LB	�FB	�XB	�wB	��B	��B	�}B	�wB	��B	��B	��B	��B	��B	B	ĜB	ƨB	ɺB	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	�B	�B	��B	�B	�B	�B	�B	�B	�B	�#B	�#B	�)B	�5B	�TB	�`B	�fB	�`B	�ZB	�ZB	�ZB	�fB	�mB	�mB	�B	�B	�B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B
	7B
oB
�B
'�B
.B
49B
6FB
@�B
E�B
K�B
P�B
VB
[#B
cTB
hsB
k�B
o�B
s�B
v�B
{�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.3 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201101452122012011014521220120110145212  AO  ARGQ                                                                        20111130144047  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130144047  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120110145212  IP                  G�O�G�O�G�O�                