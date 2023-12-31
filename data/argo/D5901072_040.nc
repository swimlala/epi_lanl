CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901072 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               (A   AO  20111130143910  20190522121828  1728_5048_040                   2C  D   APEX                            2142                            040306                          846 @Ԏ7�J?�1   @Ԏ8:� @5�|�hs�c6�+J1   GPS     Primary sampling: mixed [deeper than nominal 990dbar: discrete; nominal 990dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @���@�  A   A   A@  A`  A���A�  A�  A�  A�  A�  A�  A�  B ffBffB  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  BxffB�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C�fC  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C}�fC�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C��C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di�fDj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� D�  D�6fD�\�D��3D�fD�,�D�` D���D�� D��D��fDǳ3D���D�#3D�VfD�� D��D�  D�i�D�i�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�33@�ff@�ffA33A;33A[33A|��A���A���A���A���A͙�Aݙ�A홚A�ffB33B��B��B��B&��B.��B6��B>��BF��BN��BV��B^��Bf��Bn��Bw33B~��B�33B�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB���B���B�ffB�ffB�ffB�33B�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffC�3C�3C�3C�3C	�3C�3C�3C��C�3C�3C�3C�3C�3C�3C�3C�3C!�3C#�3C%�3C'�3C)�3C+�3C-�3C/�3C1�3C3�3C5�3C7�3C9�3C;�3C=�3C?�3CA�3CC�3CE�3CG�3CI�3CK�3CM�3CO�3CQ�3CS�3CU�3CW�3CY�3C[�3C]�3C_�3Ca�3Cc�3Ce�3Cg�3Ci�3Ck�3Cm�3Co�3Cq�3Cs�3Cu�3Cw�3Cy�3C{�3C}��C�3C�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC��fC��fC��fC�ٚC���C�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC���C�ٚC�ٚC�ٚC�ٚC�ٚC�ٚD l�D ��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D�3Dl�D��Dl�D��Dl�D��D	l�D	��D
l�D
��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��D l�D ��D!l�D!��D"l�D"��D#l�D#��D$l�D$��D%l�D%��D&l�D&��D'l�D'��D(l�D(��D)l�D)��D*l�D*��D+l�D+��D,l�D,��D-l�D-��D.l�D.��D/l�D/��D0l�D0��D1l�D1��D2l�D2��D3l�D3��D4l�D4��D5l�D5��D6l�D6��D7l�D7��D8l�D8��D9l�D9��D:l�D:��D;l�D;��D<l�D<��D=l�D=��D>l�D>��D?l�D?��D@l�D@��DAl�DA��DBl�DB��DCl�DC��DDl�DD��DEl�DE��DFl�DF��DGl�DG��DHl�DH��DIl�DI��DJl�DJ��DKl�DK��DLl�DL��DMl�DM��DNl�DN��DOl�DO��DPl�DP��DQl�DQ��DRl�DR��DSl�DS��DTl�DT��DUl�DU��DVl�DV��DWl�DW��DXl�DX��DYl�DY��DZl�DZ��D[l�D[��D\l�D\��D]l�D]��D^l�D^��D_l�D_��D`l�D`��Dal�Da��Dbl�Db��Dcl�Dc��Ddl�Dd��Del�De��Dfl�Df��Dgl�Dg��Dhl�Dh��Dis3Di��Djl�Dj��Dkl�Dk��Dll�Dl��Dml�Dm��Dnl�Dn��Dol�Do��Dpl�Dp��Dql�Dq��Drl�Dr��Dsl�Ds��Dtl�Dt��Dul�Du��Dvl�Dv��Dwl�D��fD�,�D�S3D���D���D�#3D�VfD��3D��fD� D�|�Dǩ�D��3D��D�L�D�fD�� D�fD�` D�` 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A͍PA͋DA͇+ÁA�~�A�~�ÁA�ffA�\)A�^5A�`BA�`BA�^5A�XA�ZA�\)A�ZA�ZA�ZA�ZA�XA�Q�A�M�A�I�A�C�A�/A�Aʟ�A�^5A��A�`BA�%A��yA�33A��A��yA�p�A��
A��A�jA��;A��A�M�A�ZA�
=A�jA�dZA��A���A���A�VA���A��A��A�"�A���A���A�z�A���A��DA�JA�n�A���A��7A���A�~�A��-A�VA�~�A�A��A��DA��A��A��A�z�A���A��RA��A��A�\)A���A�bA���A�~�A�S�A��A�5?A���A��A���A�A�"�A���A��mA��mA��mA���A���A�{A�/A���A�7LA��
A�bA�S�A~VA{�;Ay��Aw�Au�AsoArAq�Ak"�AhZAc�;Ac\)A_�#A]��A\�HAY�AW�mAUK�AR�jAR5?AMAL^5AK+AJ �AH��AGC�AE�mAD�/ADjAC��AA��A@��A?�mA?"�A>��A=oA;A97LA8��A6ĜA5�A3��A3&�A2z�A0-A.�A-l�A,VA+p�A*VA)��A)C�A(�A&��A$��A#�A#�7A"�`A!�#A �A ��A A�A�^A�An�A��A�HA�Al�AoA�HA��A�A�A��A�RA`BA�DAC�AbNA��A`BA�yA1'AG�AffA��A�AA�A�A	��A�!A�AE�A�FA`BA�A��AM�A �@��+@��u@�J@� �@�Ĝ@��m@�@�dZ@�O�@�1@��@���@�x�@��/@�bN@�l�@��@�z�@��@�x�@��m@��@���@ߥ�@���@�O�@�(�@��y@�M�@���@���@ٺ^@ى7@���@��
@ׅ@�33@���@Չ7@�r�@ӍP@ҏ\@�Ĝ@�@�ȴ@�
=@ϝ�@��
@�;d@��y@��@�n�@�x�@���@�9X@˾w@�K�@���@ʸR@��@��m@�p�@�=q@�
=@�O�@���@�|�@��H@���@�hs@�bN@�C�@�E�@�V@��@�l�@��y@�n�@��-@�`B@�Ĝ@�z�@��@�  @��F@���@�=q@��h@�V@��u@���@�+@��@�ȴ@�~�@�E�@��@�`B@�Ĝ@��D@���@��@��T@�p�@�%@�z�@�|�@��!@�5?@�E�@�-@���@��T@���@��h@�%@��@�r�@�bN@��D@�Ĝ@�j@���@��@���@�V@��-@�bN@�b@�l�@��@��+@�M�@��@�J@��@�@�ȴ@�K�@�"�@��@�ff@�5?@�@�{@�v�@��+@�v�@�n�@�ff@�V@��@��@��^@���@��@��7@�x�@�x�@�p�@�/@��`@���@���@���@�z�@�I�@� �@��m@�+@�n�@�-@��@��-@��@��@���@��9@��u@�bN@�  @���@�t�@�l�@�K�@�C�@�o@��H@��R@���@�=q@��-@��^@�@��#@��-@���@�7L@�Ĝ@�Q�@��;@��@�ȴ@�n�@�-@��T@��-@�X@�/@��@�%@��9@�Q�@�  @��F@���@�|�@�S�@�C�@�;d@�33@�o@���@��H@���@��y@�+@�K�@�K�@�\)@�S�@�\)@�S�@�"�@���@��+@�~�@���@�?}@��@��`@��/@��`@���@���@�Ĝ@�j@��w@�K�@���@�ȴ@��!@�~�@�5?@�@�{@��@�hs@��@�Ĝ@���@�z�@� �@��@��;@��
@��
@���@��;@���@��
@��m@��
@���@�l�@���@���@�O�@�hs@�O�@�7L@��@��@�?}@�X@�X@��@���@t9X@m@e�T@_;d@V�@OK�@H�9@Ax�@=��@9G�@4Z@/�w@)��@!X@�@  @9X@�@��@	�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A͍PA͋DA͇+ÁA�~�A�~�ÁA�ffA�\)A�^5A�`BA�`BA�^5A�XA�ZA�\)A�ZA�ZA�ZA�ZA�XA�Q�A�M�A�I�A�C�A�/A�Aʟ�A�^5A��A�`BA�%A��yA�33A��A��yA�p�A��
A��A�jA��;A��A�M�A�ZA�
=A�jA�dZA��A���A���A�VA���A��A��A�"�A���A���A�z�A���A��DA�JA�n�A���A��7A���A�~�A��-A�VA�~�A�A��A��DA��A��A��A�z�A���A��RA��A��A�\)A���A�bA���A�~�A�S�A��A�5?A���A��A���A�A�"�A���A��mA��mA��mA���A���A�{A�/A���A�7LA��
A�bA�S�A~VA{�;Ay��Aw�Au�AsoArAq�Ak"�AhZAc�;Ac\)A_�#A]��A\�HAY�AW�mAUK�AR�jAR5?AMAL^5AK+AJ �AH��AGC�AE�mAD�/ADjAC��AA��A@��A?�mA?"�A>��A=oA;A97LA8��A6ĜA5�A3��A3&�A2z�A0-A.�A-l�A,VA+p�A*VA)��A)C�A(�A&��A$��A#�A#�7A"�`A!�#A �A ��A A�A�^A�An�A��A�HA�Al�AoA�HA��A�A�A��A�RA`BA�DAC�AbNA��A`BA�yA1'AG�AffA��A�AA�A�A	��A�!A�AE�A�FA`BA�A��AM�A �@��+@��u@�J@� �@�Ĝ@��m@�@�dZ@�O�@�1@��@���@�x�@��/@�bN@�l�@��@�z�@��@�x�@��m@��@���@ߥ�@���@�O�@�(�@��y@�M�@���@���@ٺ^@ى7@���@��
@ׅ@�33@���@Չ7@�r�@ӍP@ҏ\@�Ĝ@�@�ȴ@�
=@ϝ�@��
@�;d@��y@��@�n�@�x�@���@�9X@˾w@�K�@���@ʸR@��@��m@�p�@�=q@�
=@�O�@���@�|�@��H@���@�hs@�bN@�C�@�E�@�V@��@�l�@��y@�n�@��-@�`B@�Ĝ@�z�@��@�  @��F@���@�=q@��h@�V@��u@���@�+@��@�ȴ@�~�@�E�@��@�`B@�Ĝ@��D@���@��@��T@�p�@�%@�z�@�|�@��!@�5?@�E�@�-@���@��T@���@��h@�%@��@�r�@�bN@��D@�Ĝ@�j@���@��@���@�V@��-@�bN@�b@�l�@��@��+@�M�@��@�J@��@�@�ȴ@�K�@�"�@��@�ff@�5?@�@�{@�v�@��+@�v�@�n�@�ff@�V@��@��@��^@���@��@��7@�x�@�x�@�p�@�/@��`@���@���@���@�z�@�I�@� �@��m@�+@�n�@�-@��@��-@��@��@���@��9@��u@�bN@�  @���@�t�@�l�@�K�@�C�@�o@��H@��R@���@�=q@��-@��^@�@��#@��-@���@�7L@�Ĝ@�Q�@��;@��@�ȴ@�n�@�-@��T@��-@�X@�/@��@�%@��9@�Q�@�  @��F@���@�|�@�S�@�C�@�;d@�33@�o@���@��H@���@��y@�+@�K�@�K�@�\)@�S�@�\)@�S�@�"�@���@��+@�~�@���@�?}@��@��`@��/@��`@���@���@�Ĝ@�j@��w@�K�@���@�ȴ@��!@�~�@�5?@�@�{@��@�hs@��@�Ĝ@���@�z�@� �@��@��;@��
@��
@���@��;@���@��
@��m@��
@���@�l�@���@���@�O�@�hs@�O�@�7L@��@��@�?}@�X@�X@��@���@t9X@m@e�T@_;d@V�@OK�@H�9@Ax�@=��@9G�@4Z@/�w@)��@!X@�@  @9X@�@��@	�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB1B%B+B	7B
=B
=B	7B	7B	7B1B	7B1B1B+B+B1B1B1B1B1B1B1B1B1B1B+B  B�HB��B��B��B��B�B�NB�mB�B�B�B��B  B	7BoB�B�B�B$�B?}BQ�BbNBs�Bw�B� B�Bx�Bp�B~�Bt�Bs�Bm�BcTBcTB_;BS�B@�B:^B'�B!�B�B�BhBB��B  B��B��B�B�mB�ZB�#B�B��B��B��B��B�VBo�Be`B]/B[#BC�B49B�B�B1B
�B
�NB
��B
�!B
��B
�{B
�B
t�B
]/B
H�B
$�B
VB	��B	�B	��B	��B	�-B	�XB	�B	�DB	bNB	l�B	E�B	2-B	hB	\B��B��B�sB	B��B�dB�B��B��B��B��B��B��B��B��B�oB�%B�+B�7B�=B�DB�B|�Bv�Bs�Bn�BiyBbNB^5B\)BVBT�BYBXBXBXBW
BXBZB[#BXBZB[#B^5BbNB_;B`BBbNBcTBdZBhsBdZB`BBbNBdZBe`Be`BffBdZBaHBbNBcTB`BB^5B]/B]/B^5B^5B^5B\)B`BB[#B[#BXBW
BT�BO�BN�BK�BI�BH�BG�BE�BG�BE�BH�B>wB?}B:^B7LB;dB=qB>wB=qBB�BD�BI�BE�BB�B@�B@�B@�B;dB:^B7LB?}B=qB;dB2-B7LB33B2-B2-B/B0!B0!B49B9XB?}BA�BA�B@�B@�BA�B=qBB�BJ�BO�BG�BA�BA�BC�BM�BW
BZBaHBffBjBr�Bt�Bu�Bv�Bv�Bv�Bt�Bu�Bv�Bq�Bo�Bm�BdZBdZBgmBo�Bt�BgmBe`BiyBo�Bp�BffBffBgmBcTBbNBdZBffBgmBl�Bk�Bl�Br�Bs�Bt�Bw�Bz�B�B�7B�DB�PB�VB�hB��B��B��B��B��B��B�B�'B�3B�FB�jBBɺB��B�B�/B�;B�NB�ZB�ZB�NB�TB�`B�yB�B�B��B��B��B��B	B	B	B	B	B	+B	
=B	JB	VB	oB	�B	�B	'�B	+B	0!B	33B	2-B	49B	6FB	A�B	C�B	D�B	D�B	E�B	F�B	J�B	M�B	N�B	O�B	P�B	VB	YB	ZB	\)B	^5B	aHB	aHB	bNB	gmB	jB	jB	k�B	m�B	u�B	t�B	s�B	u�B	x�B	z�B	� B	�B	�B	�B	�B	�+B	�7B	�=B	�PB	�\B	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�'B	�3B	�-B	�9B	�?B	�?B	�?B	�FB	�FB	�LB	�RB	�XB	�dB	�jB	�qB	�}B	ÖB	ŢB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�#B	�#B	�)B	�)B	�)B	�)B	�)B	�/B	�;B	�NB	�BB	�BB	�HB	�NB	�TB	�TB	�TB	�ZB	�`B	�fB	�fB	�sB	�yB	�yB	�B	�B	�B	�B	�yB	�fB	�fB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B
B
VB
�B
!�B
%�B
,B
5?B
:^B
=qB
D�B
I�B
N�B
VB
]/B
cTB
hsB
o�B
u�B
z�B
}�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B1B%B+B	7B
=B
=B
=B	7B	7B1B	7B1B1B+B+B1B1B1B1B1B1B1B1B1B1B1B	7B�B��B��B��B�
B�)B�fB�B�B��B��BB1BPB�B�B�B�B'�BB�BS�BcTBt�Bz�B�B�7B|�Bq�B�By�Bw�Br�BffBffBbNBZBD�B?}B,B#�B"�B �B�B1BBB��B��B�B�yB�sB�5B�B�B�BɺB��B�{Bs�BgmB`BB^5BH�B:^B"�B�BbB
�B
�mB
��B
�?B
��B
��B
�1B
|�B
e`B
R�B
/B
�B
B	�B	�B	ƨB	�RB	�dB	�B	��B	hsB	u�B	F�B	9XB	�B	hB	B��B�B		7B��BƨB�!B��B��B��B��B��B��B��B��B��B�7B�=B�JB�JB�bB�1B�By�By�Bs�Bn�BdZBaHBcTB[#BYB]/B[#B\)BZBYB\)B_;BbNB[#B\)B^5BbNBffB`BBbNBdZBffBgmBk�BgmBdZBdZBffBffBffBhsBhsBffBffBhsBcTBbNB`BB_;B_;B`BB`BB_;BcTB]/B]/B[#BYBZBR�BQ�BP�BK�BI�BI�BI�BL�BJ�BN�BB�BD�B>wB=qB=qB>wB?}B@�BD�BF�BJ�BF�BC�BA�BB�BB�B=qB<jB9XBA�B@�B=qB49B9XB6FB49B49B0!B1'B1'B49B:^B@�BC�BB�BA�BA�BC�B?}BD�BL�BR�BJ�BB�BA�BB�BM�BXB[#BaHBgmBl�Bs�Bu�Bv�Bw�Bw�Bw�Bw�Bx�Bv�Bv�Bt�Bo�BffBe`BhsBq�Bu�BiyBgmBk�Bq�Br�BgmBgmBhsBdZBcTBe`BgmBhsBl�Bl�Bm�Bs�Bt�Bu�Bx�B{�B�B�=B�JB�VB�\B�oB��B��B��B��B��B�B�B�-B�9B�RB�qBÖBɺB��B�#B�/B�;B�TB�`B�`B�TB�TB�`B�yB�B�B��B��B��B	  B	B	B	B	%B	%B	1B	
=B	JB	VB	oB	�B	�B	'�B	,B	1'B	49B	33B	49B	6FB	A�B	C�B	D�B	D�B	E�B	G�B	K�B	N�B	N�B	O�B	P�B	VB	YB	ZB	]/B	_;B	bNB	aHB	bNB	gmB	k�B	jB	l�B	n�B	v�B	u�B	t�B	v�B	y�B	{�B	�B	�B	�B	�B	�%B	�1B	�7B	�=B	�PB	�\B	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�B	�B	�B	�B	�-B	�9B	�3B	�9B	�?B	�?B	�?B	�FB	�FB	�LB	�RB	�XB	�dB	�jB	�qB	�}B	ÖB	ŢB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	�
B	�B	�B	�#B	�#B	�)B	�)B	�/B	�)B	�/B	�/B	�)B	�/B	�;B	�TB	�HB	�BB	�HB	�TB	�ZB	�ZB	�TB	�ZB	�`B	�fB	�fB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�mB	�fB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B
%B
VB
�B
!�B
%�B
,B
5?B
;dB
=qB
D�B
I�B
O�B
VB
]/B
cTB
hsB
o�B
v�B
z�B
}�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.3 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201101452072012011014520720120110145207  AO  ARGQ                                                                        20111130143910  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130143910  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120110145207  IP                  G�O�G�O�G�O�                