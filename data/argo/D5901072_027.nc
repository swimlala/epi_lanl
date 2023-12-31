CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS      N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-28T23:01:46Z UW 3.1 conversion   
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
_FillValue        G�O�   axis      Z           9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�        C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�        M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�        U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�        _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�        i�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�        q�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    y�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�        {�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�        ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �    HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �L   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �\   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �`   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        �p   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        �t   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �x   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �|Argo profile    3.1 1.2 19500101000000  5901072 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               A   AO  20111130143746  20190522121828  1728_5048_027                   2C  D   APEX                            2142                            040306                          846 @�m�!��1   @�m��Q�@6E�Q��cL1&�x�1   GPS     Primary sampling: mixed [deeper than nominal 990dbar: discrete; nominal 990dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @���@�  A   A   A>ffA`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CC�fCF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;y�D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC�fDD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DPy�DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd�fDe  De� Df  Df�fDg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Do��Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� DyffD���D�@ D�vfD��3D��3D�6fD�vfD��fD��fD�fD�p DǙ�D�� D��D�&fD�� D��3D� D�9�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�  @�ff@�ffA33A9��A[33A{33A���A���A���A���A͙�Aݙ�A홚A���B��B��B��B��B&��B.��B6��B>��BF��BN��BV��B^��Bf��Bn��Bv��B~��B���B�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffC�3C�3C�3C�3C	�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C!�3C#�3C%�3C'�3C)�3C+�3C-�3C/�3C1�3C3�3C5�3C7�3C9�3C;�3C=�3C?�3CA�3CC��CE�3CG�3CI�3CK�3CM�3CO�3CQ�3CS�3CU�3CW�3CY�3C[�3C]�3C_�3Ca�3Cc�3Ce�3Cg�3Ci�3Ck�3Cm�3Co�3Cq�3Cs�3Cu�3Cw�3Cy�3C{�3C}�3C�3C�ٚC�ٚC�ٚC�ٚC�ٚC���C�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC���C�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC��fC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC��fC��fC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC���C�ٚC�ٚC�ٚC��fC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚD l�D ��DffD��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��D	l�D	��D
l�D
��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��D l�D ��D!l�D!��D"l�D"��D#l�D#��D$l�D$��D%l�D%��D&l�D&��D'l�D'��D(l�D(��D)l�D)��D*l�D*��D+l�D+��D,l�D,��D-l�D-��D.l�D.��D/l�D/��D0l�D0��D1l�D1��D2l�D2��D3l�D3��D4l�D4��D5l�D5��D6l�D6��D7l�D7��D8l�D8��D9l�D9��D:l�D:��D;ffD;��D<l�D<��D=l�D=��D>l�D>��D?l�D?��D@l�D@��DAl�DA��DBl�DB��DCs3DC��DDl�DD��DEl�DE��DFl�DF��DGl�DG��DHl�DH��DIl�DI��DJl�DJ��DKl�DK��DLl�DL��DMl�DM��DNl�DN��DOl�DO��DPffDP��DQl�DQ��DRl�DR��DSl�DS��DTl�DT��DUl�DU��DVl�DV��DWl�DW��DXl�DX��DYl�DY��DZl�DZ��D[l�D[��D\l�D\��D]l�D]��D^l�D^��D_l�D_��D`l�D`��Dal�Da��Dbl�Db��Dcl�Dc��Dds3Dd��Del�De��Dfs3Df��Dgl�Dg��Dhl�Dh��Dil�Di��Djl�Dj��Dkl�Dk��Dll�Dl��Dml�Dm��Dnl�Dn��Dol�Do�fDpl�Dp��Dql�Dq��Drl�Dr��Dsl�Ds��Dtl�Dt��Dul�Du��Dvl�DyS3D�� D�6fD�l�D���D��D�,�D�l�D���D���D���D�ffDǐ D��fD�3D��D�fD�ٚD�fD�0 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A���A��A��A��HA��TA��/A��A��
A��
A���A���A�ĜA�A��RA��A���A�v�A��A���A�jA�VA��A�ƨA��A��7A�\)A���A��9A�n�A�S�A�K�A�E�A�7LA�=qA�33A�oA�A���A���A�K�A�|�A��A��\A�;dA��A���A�ȴA�ĜA��jA��!A���A�t�A�A��uA�hsA��A��yA��^A��A��A�n�A��HA�O�A��;A��\A�jA�\)A�=qA�(�A�A�S�A���A�$�A�E�A�?}A�{A�/A�v�A���A��A�-A�O�A�n�A�S�A�M�A��FA�r�A�Q�A�?}A�(�A���A�x�A��FA���A�oA��hA�^5A���A��PA�^5A��A�A�-A���A��
A�l�A���A�E�A��hA�t�A�n�A�^5A~$�A{O�Ax�\AwK�Au��Au%Ast�Ar�/Ar^5Aq��Aq;dAp��Ao�mAm��AjE�AiG�Ahr�Ae�hAdZAc�AbQ�Aa��A_�#A^bA\��AZ�AX�/AU�mAS�AR�AQ33AO��AM��AL�AL��AL�9ALffAK\)AJE�AI\)AG�AFz�ACoA?A=G�A;�A;%A:=qA9VA7�mA4�A3K�A1��A1
=A0�HA0�RA0^5A05?A0A/x�A.E�A,��A*��A)��A(=qA'&�A&�uA&�A%�FA$��A#�A!oA 1'A 1Ax�A~�A��A�
A`BA�yA�hA��A��A"�A��AffAJA�^AoAZA��A?}A�RA/A�#A�A��A�/A	��A�
AA�HAQ�A�TA�AE�AK�A"�A �A n�@��@�G�@�Q�@�
=@��7@��@���@��y@�@� �@�$�@�A�@�C�@�M�@�F@�@�X@��@��@�  @�S�@�@��H@���@噚@�\@�t�@݉7@��@�ƨ@�dZ@��y@���@ڇ+@�^5@�{@١�@�&�@�9X@��@��#@�z�@�"�@Ұ!@�v�@�J@Ѓ@�|�@Ο�@���@��@�7L@��@�j@��;@��@��@��@�t�@͡�@�v�@�ff@���@Χ�@�n�@��
@�;d@�@���@�9X@�=q@Ȭ@���@�@�x�@��`@�Q�@î@�\)@���@�ȴ@°!@+@�@�@§�@���@�?}@���@�~�@�hs@�?}@�Z@���@��@�/@�(�@�b@�1@��@�9X@��m@�~�@���@���@���@�o@��y@���@��@��j@��@���@��@���@�-@��-@��7@�p�@�G�@���@�r�@�I�@�9X@�1'@�1'@� �@���@�ȴ@�5?@���@���@��^@��T@��@��@��T@��^@�x�@���@�9X@���@��@��w@�|�@���@��7@�O�@�G�@�/@�%@���@���@���@�bN@�A�@� �@���@�+@���@�@�p�@��@���@��-@���@�@��^@�@���@��7@�X@�%@��/@�%@��@��`@���@�z�@�(�@���@��@�;d@�33@�"�@�o@���@��H@���@�5?@��^@��@�z�@���@���@���@��w@��@��@�I�@�1@��@��w@�+@��^@�j@���@��/@��u@���@��@��@��P@��F@��H@���@��+@�v�@�M�@�-@��@�hs@��@�Ĝ@�z�@�Z@��@��@��@�|�@�S�@��R@�V@�=q@��@���@���@�O�@���@��D@�Q�@��m@�|�@��@��\@�5?@��@�M�@�n�@���@��w@��@�b@�(�@�Q�@���@��@�G�@��j@���@���@�A�@�A�@��@���@���@���@�~�@��@���@���@{t�@qX@kƨ@a7L@Z=q@R�\@J��@FE�@@  @;��@3"�@.V@(�9@#�
@�m@5?@%@�-11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��A���A��A��A��HA��TA��/A��A��
A��
A���A���A�ĜA�A��RA��A���A�v�A��A���A�jA�VA��A�ƨA��A��7A�\)A���A��9A�n�A�S�A�K�A�E�A�7LA�=qA�33A�oA�A���A���A�K�A�|�A��A��\A�;dA��A���A�ȴA�ĜA��jA��!A���A�t�A�A��uA�hsA��A��yA��^A��A��A�n�A��HA�O�A��;A��\A�jA�\)A�=qA�(�A�A�S�A���A�$�A�E�A�?}A�{A�/A�v�A���A��A�-A�O�A�n�A�S�A�M�A��FA�r�A�Q�A�?}A�(�A���A�x�A��FA���A�oA��hA�^5A���A��PA�^5A��A�A�-A���A��
A�l�A���A�E�A��hA�t�A�n�A�^5A~$�A{O�Ax�\AwK�Au��Au%Ast�Ar�/Ar^5Aq��Aq;dAp��Ao�mAm��AjE�AiG�Ahr�Ae�hAdZAc�AbQ�Aa��A_�#A^bA\��AZ�AX�/AU�mAS�AR�AQ33AO��AM��AL�AL��AL�9ALffAK\)AJE�AI\)AG�AFz�ACoA?A=G�A;�A;%A:=qA9VA7�mA4�A3K�A1��A1
=A0�HA0�RA0^5A05?A0A/x�A.E�A,��A*��A)��A(=qA'&�A&�uA&�A%�FA$��A#�A!oA 1'A 1Ax�A~�A��A�
A`BA�yA�hA��A��A"�A��AffAJA�^AoAZA��A?}A�RA/A�#A�A��A�/A	��A�
AA�HAQ�A�TA�AE�AK�A"�A �A n�@��@�G�@�Q�@�
=@��7@��@���@��y@�@� �@�$�@�A�@�C�@�M�@�F@�@�X@��@��@�  @�S�@�@��H@���@噚@�\@�t�@݉7@��@�ƨ@�dZ@��y@���@ڇ+@�^5@�{@١�@�&�@�9X@��@��#@�z�@�"�@Ұ!@�v�@�J@Ѓ@�|�@Ο�@���@��@�7L@��@�j@��;@��@��@��@�t�@͡�@�v�@�ff@���@Χ�@�n�@��
@�;d@�@���@�9X@�=q@Ȭ@���@�@�x�@��`@�Q�@î@�\)@���@�ȴ@°!@+@�@�@§�@���@�?}@���@�~�@�hs@�?}@�Z@���@��@�/@�(�@�b@�1@��@�9X@��m@�~�@���@���@���@�o@��y@���@��@��j@��@���@��@���@�-@��-@��7@�p�@�G�@���@�r�@�I�@�9X@�1'@�1'@� �@���@�ȴ@�5?@���@���@��^@��T@��@��@��T@��^@�x�@���@�9X@���@��@��w@�|�@���@��7@�O�@�G�@�/@�%@���@���@���@�bN@�A�@� �@���@�+@���@�@�p�@��@���@��-@���@�@��^@�@���@��7@�X@�%@��/@�%@��@��`@���@�z�@�(�@���@��@�;d@�33@�"�@�o@���@��H@���@�5?@��^@��@�z�@���@���@���@��w@��@��@�I�@�1@��@��w@�+@��^@�j@���@��/@��u@���@��@��@��P@��F@��H@���@��+@�v�@�M�@�-@��@�hs@��@�Ĝ@�z�@�Z@��@��@��@�|�@�S�@��R@�V@�=q@��@���@���@�O�@���@��D@�Q�@��m@�|�@��@��\@�5?@��@�M�@�n�@���@��w@��@�b@�(�@�Q�@���@��@�G�@��j@���@���@�A�@�A�@��@���@���@���@�~�@��@���@���@{t�@qX@kƨ@a7L@Z=q@R�\@J��@FE�@@  @;��@3"�@.V@(�9@#�
@�m@5?@%@�-11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�B��B��B�B�B�B�B�B�
B�
B�
B�
B�
B�B�B�B�B�#B�`B��B��BBPB�B �B%�B-BC�BaHBr�Bq�Bq�Bq�Br�By�B�VB��B��B��B�!B�dBȴB��B�#B�TB�`B�`B�fB�fB�mB�yB�B�B�B�B�B�B�B�yB�B�NB�B�B�B�B�;B�5B�5B�5B�5B�B�wB�dB�9B�B�B��B�7BhsBT�BR�BL�B=qB-B!�BuBDB1BBBBB�B��B��B�Bv�BZBG�B7LB�BhB  B
��B
�LB
�B
�+B
}�B
u�B
jB
XB
H�B
6FB
(�B
hB
  B	�B	�sB	�TB	�B	��B	��B	��B	��B	��B	��B	�'B	�!B	��B	��B	�DB	�JB	�B	}�B	u�B	jB	^5B	Q�B	I�B	5?B	�B	oB	
=B	%B	B	%B	%B	�B	#�B	!�B	�B	�B	JB	1B��B�B�BǮB��B�wB�qB�jB�!B�B��B�B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�oB�VB�bB�+B�%B�B�B�B~�B}�B|�B{�Bx�Bv�Bw�Bt�Bt�Bs�Br�Bq�Bp�Bn�Bm�Bl�BiyBjBn�BbNB_;BdZB\)BbNBXBT�BVBT�BK�BM�BM�BF�BC�BB�BG�BA�B?}B=qB=qB>wB=qB;dB<jBB�B?}B8RB5?B0!B/B-B,B,B/B8RB=qB>wB>wB?}BC�BL�BZBYBS�BR�BW
B[#B_;B_;B_;B`BBbNBdZBgmBhsBhsBk�Bo�Bq�Br�Br�Bu�Bs�Bs�Bu�Bx�Bw�B{�B�B�7B�PB�hB�oB��B��B�B�9B�LB�jBĜBƨB�B�`B�)B�)B�B�B��B��B��B��B�B�#B�#B�)B�/B�HB�ZB�fB�B��B��B��B��B��B�sB�yB�yB�B�B�B�B�B��B��B��B	B	PB	\B	DB	
=B	VB	�B	$�B	�B	�B	'�B	#�B	�B	"�B	'�B	"�B	 �B	"�B	$�B	)�B	+B	-B	-B	-B	.B	1'B	8RB	9XB	=qB	@�B	A�B	B�B	D�B	F�B	H�B	I�B	K�B	M�B	N�B	R�B	P�B	R�B	XB	[#B	^5B	bNB	cTB	dZB	e`B	e`B	e`B	ffB	ffB	gmB	iyB	k�B	m�B	p�B	q�B	q�B	r�B	v�B	y�B	z�B	{�B	|�B	|�B	}�B	� B	�B	�B	�B	�%B	�1B	�=B	�PB	�VB	�\B	�bB	�hB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�{B	�uB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�9B	�?B	�LB	�dB	�wB	��B	��B	��B	��B	��B	��B	��B	ÖB	ÖB	ÖB	ÖB	ĜB	ŢB	ŢB	ƨB	ƨB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�5B	�BB	�BB	�BB	�HB	�TB	�sB	�B	�B	�sB	�B	�B	�B	�B	�B	�B	��B	��B	��B	�B	��B
+B
\B
 �B
-B
33B
:^B
@�B
E�B
H�B
N�B
T�B
ZB
_;B
cTB
jB
o�B
s�B
w�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�B��B��B�B�B�B�B�B�
B�
B�
B�
B�
B�B�B�B�B�/B�mB��B��BB\B�B!�B&�B/BD�BbNBs�Bq�Bq�Bq�Br�By�B�\B��B��B��B�'B�}B��B��B�/B�`B�fB�`B�fB�fB�mB�yB�B��B��B�B�B�B�B�B�B�yB�B�B�B�B�BB�5B�;B�5B�;B�HB��B�qB�RB�9B�!B��B�hBk�BW
BT�BP�BF�B33B&�B�BJB	7BBBBB��B��B��B�PB}�B^5BM�B>wB#�B�BoB
�NB
�wB
�9B
�JB
�B
z�B
q�B
_;B
O�B
?}B
2-B
�B
B	��B	�B	�sB	�B	�
B	��B	��B	��B	��B	ƨB	�RB	�-B	�B	��B	�VB	�\B	�B	� B	z�B	o�B	bNB	W
B	P�B	>wB	&�B	{B	\B	JB		7B	
=B	+B	�B	$�B	%�B	"�B	�B	hB	PB	B��B�/B��BB��B��B�}B�LB�!B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�hB�{B�\B�7B�%B�%B�%B�B�B~�B}�B}�By�Bz�Bv�Bv�Bt�Bs�Br�Br�Bq�Bp�Bn�Bk�Bo�Br�Be`BcTBgmBbNBffB\)BW
BW
BVBL�BP�BP�BG�BD�BC�BI�BD�BA�B?}B?}B@�B>wB=qB>wBE�BB�B;dB7LB2-B33B0!B-B-B0!B:^B>wB?}B?}B@�BE�BQ�B_;B]/BW
BS�BXB\)B`BB`BB`BB`BBcTBe`BiyBk�BjBn�Bq�Br�Bs�Bs�Bx�Bu�Bu�Bw�By�Bw�B|�B�B�=B�\B�oB�oB�{B��B�B�9B�LB�jBĜBĜB�B�sB�5B�5B�5B�#B�
B�B��B��B�
B�)B�)B�/B�/B�HB�ZB�fB�B��B��B��B��B	  B�B�B�B�B�B�B�B�B��B��B��B	B	\B	bB	DB	
=B	PB	�B	%�B	�B	 �B	(�B	$�B	�B	$�B	(�B	#�B	 �B	"�B	%�B	+B	,B	-B	-B	-B	.B	1'B	9XB	;dB	>wB	A�B	A�B	B�B	D�B	F�B	H�B	I�B	L�B	N�B	O�B	S�B	Q�B	R�B	YB	\)B	_;B	dZB	dZB	dZB	e`B	e`B	e`B	ffB	gmB	hsB	iyB	k�B	n�B	q�B	r�B	s�B	s�B	v�B	y�B	z�B	{�B	|�B	|�B	}�B	� B	�B	�B	�B	�%B	�1B	�=B	�VB	�VB	�bB	�hB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�uB	�uB	�{B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	�B	�B	�B	�FB	�?B	�LB	�jB	�}B	��B	��B	��B	��B	��B	��B	B	ÖB	ĜB	ÖB	ĜB	ĜB	ƨB	ƨB	ǮB	ǮB	ɺB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�5B	�BB	�BB	�BB	�HB	�TB	�sB	�B	�B	�sB	�B	�B	�B	�B	��B	�B	��B	��B	��B	�B	��B
+B
\B
 �B
-B
33B
:^B
@�B
E�B
H�B
N�B
T�B
ZB
_;B
cTB
jB
o�B
s�B
x�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<�C�<T��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.3 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201101452022012011014520220120110145202  AO  ARGQ                                                                        20111130143746  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130143746  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120110145202  IP                  G�O�G�O�G�O�                