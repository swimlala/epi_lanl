CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901072 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               A   AO  20111130143759  20190522121828  1728_5048_029                   2C  D   APEX                            2142                            040306                          846 @�r��C�1   @�r�y\��@5ѩ��l��cR�x���1   GPS     Primary sampling: mixed [deeper than nominal 990dbar: discrete; nominal 990dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�ff@�  A   A   A@  A^ffA�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B���B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B���B���C�fC  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CS�fCU�fCX  CZ�C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'�fD(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1y�D1��D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh�fDi  Di� Dj  Dj�fDkfDk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dwl�Dy�fD�	�D�6fD�p D��3D�	�D�&fD���D��fD��D�  D�\�DǦfD��3D�&fD�vfD� D���D��D�Ff1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @y��@�ff@�ffA33A;33AY��A{33A���A���A���A���A͙�Aݙ�A홚A���B��B��B��B��B&��B.��B6��B>��BF��BN��BV��B^��Bf��Bn��Bv��B~��B�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�33B�ffB�ffB�ffB�ffB�33B�ffB�ffB�ffB�ffB�33B�ffB�ffB�ffB�ffB�ffB�ffB�33B�33C��C�3C�3C�3C	�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C!�3C#�3C%�3C'�3C)�3C+�3C-�3C/�3C1�3C3�3C5�3C7�3C9�3C;�3C=�3C?�3CA�3CC�3CE�3CG�3CI�3CK�3CM�3CO�3CQ�3CS��CU��CW�3CY��C[�3C]�3C_�3Ca�3Cc�3Ce�3Cg�3Ci�3Ck�3Cm�3Co�3Cq�3Cs�3Cu�3Cw�3Cy�3C{�3C}�3C�3C�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC���C�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC��fC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC���C���C�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC���C���C�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚD l�D ��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��D	l�D	��D
l�D
��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��D l�D ��D!l�D!��D"l�D"��D#l�D#��D$l�D$��D%l�D%��D&l�D&��D's3D'��D(l�D(��D)l�D)��D*l�D*��D+l�D+��D,l�D,��D-l�D-��D.l�D.��D/l�D/��D0l�D0��D1ffD1�fD2l�D2��D3l�D3��D4l�D4��D5l�D5��D6l�D6��D7l�D7��D8l�D8��D9l�D9��D:l�D:��D;l�D;��D<l�D<��D=l�D=��D>l�D>��D?l�D?��D@l�D@��DAl�DA��DBl�DB��DCl�DC��DDl�DD��DEl�DE��DFl�DF��DGl�DG��DHl�DH��DIl�DI��DJl�DJ��DKl�DK��DLl�DL��DMl�DM��DNl�DN��DOl�DO��DPl�DP��DQl�DQ��DRl�DR��DSl�DS��DTl�DT��DUl�DU��DVl�DV��DWl�DW��DXl�DX��DYl�DY��DZl�DZ��D[l�D[��D\l�D\��D]l�D]��D^l�D^��D_l�D_��D`l�D`��Dal�Da��Dbl�Db��Dcl�Dc��Ddl�Dd��Del�De��Dfl�Df��Dgl�Dg��Dhs3Dh��Dil�Di��Djs3Dj�3Dkl�Dk��Dll�Dl��Dml�Dm��Dnl�Dn��Dol�Do��Dpl�Dp��Dql�Dq��Drl�Dr��Dsl�Ds��Dtl�Dt��Dul�Du��Dvl�Dv��DwY�Dy�3D�  D�,�D�ffD���D�  D��D�� D���D�� D�fD�S3Dǜ�D��D��D�l�D��fD��3D� D�<�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A��A�%A��;A�1'A���A��A�v�A�E�A�  A���A��jA���A�C�A�jA���A��A��A��HA��-A�/A�
=A��\A��#A���A�jA��
A�M�A���A�Q�A�K�A�t�A�VA�hsA���A��A��wA�ffA�;dA��A�ƨA��A��!A��PA�t�A�A���A�1'A�(�A���A��A���A�=qA�1A��9A�z�A�/A�S�A�  A���A�M�A��`A���A�ZA�/A��mA��A��^A�ZA�33A�S�A���A�A�A��9A�7LA�oA�v�A�$�A��A��`A��#A���A��TA�hsA���A�t�A�{A���A�M�A��A���A�bA�~�A�A��A�1A��A���A�C�A��A�
=A�v�A��!A�l�A��A��A��uA��A}XAz~�Ax  Av�DAsx�Aq��An��Am/AlM�Ah{Aa��A`��A`Q�A_`BA]�TA[|�AYK�AUO�AQAP9XAOƨAOt�AOS�ANAL��AK��AJ�RAI?}AHE�AG\)AE�AA��A>�HA=��A=C�A<bNA;t�A:VA9��A8�A7�;A6I�A4�`A4��A41A3��A3�A2��A2�A0�+A//A.JA-+A,$�A, �A*�A)��A)�A*-A(�A&~�A%`BA$n�A#�A#�A#%A!�;A �RA�A1'A+A�9Az�A^5A+An�A��A�A�A^5A�#A�-Ax�A��Ar�A��At�A��A��A5?A��A^5A5?AȴA9XAJA��AƨAC�A	ƨA�/A��A�TA
=A��A��A$�AA��A7LA/A Z@�@���@���@��`@��9@�z�@�K�@���@��@�7@���@���@�K�@�V@��`@�(�@�=q@�z�@ߍP@�@�G�@ۮ@��@��@�bN@�ƨ@�C�@Ցh@�Z@��m@�C�@�E�@Л�@�|�@Η�@́@�j@�K�@�$�@��`@ǍP@�V@��@ċD@�|�@�@���@Ǯ@Ɵ�@��@�bN@Õ�@��@�J@�@�x�@�hs@�p�@�p�@���@��m@��P@���@���@��-@�r�@��@���@��@��+@�J@��h@�hs@�&�@���@�r�@��R@��#@���@�x�@�Ĝ@��
@�-@��7@�p�@�O�@���@���@���@�\)@���@�ff@��@���@�b@��j@�/@���@�z�@���@�x�@�Ĝ@��y@��h@�/@���@�r�@�  @�ƨ@��@�A�@�j@�Z@��
@���@�t�@�t�@�t�@�\)@�l�@�dZ@��P@��@�^5@��@�S�@�l�@�K�@��H@�E�@��7@���@�l�@�-@��@���@�o@���@�=q@�7L@��@�1'@�"�@��#@�/@���@��@�X@�7L@���@��@���@�C�@��R@�~�@�;d@���@���@��@�ff@�{@�@���@�&�@�%@��@�`B@���@�=q@��!@��+@��@��T@��+@�S�@�33@�o@�|�@�C�@��y@�^5@�{@�hs@�/@�7L@��@��j@�bN@�  @��@��@��@��T@�-@���@��H@��P@�t�@���@�&�@���@��R@�~�@�5?@��@���@�p�@��@�x�@�p�@�x�@��@���@�r�@�z�@�V@��j@�Z@�(�@��@�n�@�p�@�?}@�V@���@��/@��9@��D@�1@��@��H@�+@�\)@�+@���@�5?@��-@��h@�x�@�&�@�%@�Ĝ@�r�@�(�@���@���@��@���@��P@�t�@�dZ@�+@���@��y@��H@�ȴ@��+@�E�@�{@���@��@��T@���@�@��^@���@��7@�O�@�V@��j@���@�%@��@��h@��D@K�@v��@nV@e��@\�j@T��@L��@EV@>�@6��@1&�@+dZ@&ff@ b@z�@�y@��@l�@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��A��A�%A��;A�1'A���A��A�v�A�E�A�  A���A��jA���A�C�A�jA���A��A��A��HA��-A�/A�
=A��\A��#A���A�jA��
A�M�A���A�Q�A�K�A�t�A�VA�hsA���A��A��wA�ffA�;dA��A�ƨA��A��!A��PA�t�A�A���A�1'A�(�A���A��A���A�=qA�1A��9A�z�A�/A�S�A�  A���A�M�A��`A���A�ZA�/A��mA��A��^A�ZA�33A�S�A���A�A�A��9A�7LA�oA�v�A�$�A��A��`A��#A���A��TA�hsA���A�t�A�{A���A�M�A��A���A�bA�~�A�A��A�1A��A���A�C�A��A�
=A�v�A��!A�l�A��A��A��uA��A}XAz~�Ax  Av�DAsx�Aq��An��Am/AlM�Ah{Aa��A`��A`Q�A_`BA]�TA[|�AYK�AUO�AQAP9XAOƨAOt�AOS�ANAL��AK��AJ�RAI?}AHE�AG\)AE�AA��A>�HA=��A=C�A<bNA;t�A:VA9��A8�A7�;A6I�A4�`A4��A41A3��A3�A2��A2�A0�+A//A.JA-+A,$�A, �A*�A)��A)�A*-A(�A&~�A%`BA$n�A#�A#�A#%A!�;A �RA�A1'A+A�9Az�A^5A+An�A��A�A�A^5A�#A�-Ax�A��Ar�A��At�A��A��A5?A��A^5A5?AȴA9XAJA��AƨAC�A	ƨA�/A��A�TA
=A��A��A$�AA��A7LA/A Z@�@���@���@��`@��9@�z�@�K�@���@��@�7@���@���@�K�@�V@��`@�(�@�=q@�z�@ߍP@�@�G�@ۮ@��@��@�bN@�ƨ@�C�@Ցh@�Z@��m@�C�@�E�@Л�@�|�@Η�@́@�j@�K�@�$�@��`@ǍP@�V@��@ċD@�|�@�@���@Ǯ@Ɵ�@��@�bN@Õ�@��@�J@�@�x�@�hs@�p�@�p�@���@��m@��P@���@���@��-@�r�@��@���@��@��+@�J@��h@�hs@�&�@���@�r�@��R@��#@���@�x�@�Ĝ@��
@�-@��7@�p�@�O�@���@���@���@�\)@���@�ff@��@���@�b@��j@�/@���@�z�@���@�x�@�Ĝ@��y@��h@�/@���@�r�@�  @�ƨ@��@�A�@�j@�Z@��
@���@�t�@�t�@�t�@�\)@�l�@�dZ@��P@��@�^5@��@�S�@�l�@�K�@��H@�E�@��7@���@�l�@�-@��@���@�o@���@�=q@�7L@��@�1'@�"�@��#@�/@���@��@�X@�7L@���@��@���@�C�@��R@�~�@�;d@���@���@��@�ff@�{@�@���@�&�@�%@��@�`B@���@�=q@��!@��+@��@��T@��+@�S�@�33@�o@�|�@�C�@��y@�^5@�{@�hs@�/@�7L@��@��j@�bN@�  @��@��@��@��T@�-@���@��H@��P@�t�@���@�&�@���@��R@�~�@�5?@��@���@�p�@��@�x�@�p�@�x�@��@���@�r�@�z�@�V@��j@�Z@�(�@��@�n�@�p�@�?}@�V@���@��/@��9@��D@�1@��@��H@�+@�\)@�+@���@�5?@��-@��h@�x�@�&�@�%@�Ĝ@�r�@�(�@���@���@��@���@��P@�t�@�dZ@�+@���@��y@��H@�ȴ@��+@�E�@�{@���@��@��T@���@�@��^@���@��7@�O�@�V@��j@���@�%@��@��h@��D@K�@v��@nV@e��@\�j@T��@L��@EV@>�@6��@1&�@+dZ@&ff@ b@z�@�y@��@l�@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B��B�9B�wBɺB��B��B�B�#B�#B�B��B��B��B�)B�B�B��BB  B��B��BPBbBPBJBJBJBDBB��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�`B�ZB�#B��B��B��BȴBƨBB�qB�?B�3B�B��B��B��B��B��B��B��B�DB�DB�Bw�Bt�Bl�BcTBW
B=qB#�BDB��B�TB��B��B�B�B��B��B�bB�7B�B� Bx�Bn�BbNBXBC�B$�B�BB
��B
�NB
��B
��B
��B
�bB
y�B
^5B
I�B
?}B
0!B
�B	��B	�B	�B	��B	�B	�B	�B	}�B	D�B	>wB	:^B	1'B	/B	"�B	
=B�B�B�B�B��B��B��B��BȴBɺB�B�B��B��B�B��B�hB�\B�PB�B�B~�B{�Bz�Bz�Bv�Bt�Bw�Br�Bq�Bv�Bn�BjBu�Bt�Bo�Bq�BdZBm�B|�B�B�=B�JB�\B�+B�%B�B~�B{�By�Bx�Bt�Bv�Bs�Bs�Bu�Bs�Bo�Bo�Bm�BjBk�BhsBiyBiyBiyBhsBgmBffBffBe`BdZBcTBbNB]/BaHBW
BVBT�BS�BR�BO�BM�BM�BL�BI�BI�BI�BI�BH�BG�BF�BH�BB�BC�BK�BD�B5?B2-B1'B0!B33B0!B+B.B)�B6FB$�B#�B+B!�B"�B"�B"�B �B�B�B�B�B�B�B�B�B�B�B�B�B�B �B$�B!�B!�B%�B"�B�B#�B&�B'�B'�B0!BA�BR�B^5B]/B]/B]/B^5BbNBjBr�Bt�Bu�Bv�Bw�B~�B�B�1B�\B�\B�bB�uB�uB��B��B��B��B��B��B��B��B��B�!B�-B�3B�9B�?B�RB�^B�qB�}BBĜBŢB��B�jBÖB��B�
B�TB�B��B��B	B	1B	oB	 �B	)�B	!�B	!�B	&�B	+B	,B	-B	/B	49B	:^B	>wB	A�B	D�B	E�B	G�B	I�B	K�B	L�B	Q�B	T�B	T�B	_;B	p�B	s�B	u�B	v�B	y�B	{�B	}�B	z�B	r�B	n�B	iyB	e`B	gmB	dZB	e`B	ffB	cTB	bNB	`BB	^5B	\)B	\)B	ZB	[#B	XB	XB	YB	XB	YB	[#B	\)B	`BB	ffB	iyB	k�B	l�B	k�B	l�B	m�B	o�B	r�B	r�B	t�B	y�B	~�B	�B	�7B	�JB	�7B	�7B	�JB	�hB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�'B	�3B	�LB	ǮB	�qB	�?B	�?B	�!B	�'B	�-B	�'B	�-B	�-B	�9B	�9B	�?B	�FB	�LB	ÖB	��B	��B	�
B	�#B	�)B	�/B	�/B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�#B	�)B	�)B	�)B	�/B	�/B	�5B	�5B	�;B	�BB	�5B	�5B	�5B	�HB	�BB	�BB	�BB	�HB	�HB	�HB	�HB	�HB	�NB	�NB	�ZB	�fB	�mB	�mB	�B	�B	�B	��B
B
DB
�B
#�B
-B
49B
;dB
B�B
F�B
M�B
R�B
YB
^5B
cTB
iyB
l�B
p�B
s�B
u�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B��B��B��B��B�?B��B��B��B��B�
B�#B�)B�/B��BÖB��B�)B�B�B��BBB��B  B\BoB\BVBVBoBVBBBB��B��B��B��B��B��B��B��B�B�B�B�B�B�B�sB�;B�B��B��B��BȴBŢBÖB�RB�FB�!B�B��B��B��B��B��B��B�PB�JB�%Bz�Bw�Bo�Be`B\)BD�B)�BhB��B�sB�
BŢB�-B�B��B��B�uB�DB�B�B|�Bq�Be`B_;BN�B+B!�B1B
��B
�B
ɺB
�B
��B
��B
�B
dZB
M�B
H�B
9XB
�B	��B	��B	�B	��B	�-B	�B	�RB	�VB	F�B	?}B	=qB	5?B	5?B	(�B	�B��B�B�)B�
B�B�
B��B��B��B��B�B�)B�B�B�RB��B�{B�oB�bB�1B�B�B� B�B� Bw�Bv�By�Bt�Bs�Bx�Br�Bm�Bx�Bv�Br�Bq�BiyBn�B|�B�B�PB�oB�oB�=B�1B�B�B~�B}�B}�Bw�By�Bu�Bt�Bv�Bw�Br�Bq�Bp�Bn�Bp�BjBjBjBk�BjBiyBhsBhsBgmBffBe`BhsBe`BffBYBW
BVBT�BT�BT�BP�BN�BO�BL�BJ�BJ�BK�BJ�BJ�BL�BO�BE�BH�BP�BH�B;dB33B2-B2-B5?B49B-B/B.B9XB&�B%�B,B$�B%�B$�B#�B#�B"�B�B�B�B�B�B �B�B�B�B�B"�B�B"�B&�B#�B#�B'�B%�B"�B%�B'�B+B)�B,B>wBS�B`BB_;B`BB_;B`BBdZBk�Bs�Bt�Bu�Bv�Bx�B�B�B�7B�bB�hB�oB�{B�{B��B��B��B��B��B��B��B��B��B�'B�3B�9B�?B�LB�dB�dB�qB��BÖBƨBȴBĜB�qBĜB��B�B�NB�B��B��B	%B	1B	hB	"�B	.B	$�B	"�B	'�B	,B	-B	.B	/B	49B	:^B	>wB	B�B	D�B	E�B	G�B	I�B	K�B	L�B	Q�B	T�B	S�B	\)B	o�B	s�B	u�B	v�B	z�B	|�B	~�B	|�B	t�B	p�B	iyB	e`B	hsB	e`B	ffB	hsB	dZB	cTB	bNB	`BB	^5B	_;B	]/B	\)B	YB	YB	ZB	ZB	ZB	\)B	]/B	_;B	ffB	iyB	l�B	n�B	l�B	l�B	n�B	p�B	r�B	r�B	t�B	x�B	}�B	�B	�=B	�PB	�7B	�1B	�DB	�hB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�'B	�-B	�LB	ȴB	�}B	�LB	�FB	�'B	�-B	�3B	�-B	�3B	�-B	�9B	�9B	�?B	�?B	�?B	B	��B	��B	�B	�)B	�/B	�5B	�;B	�#B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	�B	�B	�B	�#B	�#B	�/B	�)B	�)B	�/B	�/B	�5B	�;B	�;B	�BB	�5B	�5B	�;B	�NB	�BB	�BB	�BB	�HB	�HB	�HB	�HB	�HB	�NB	�TB	�`B	�mB	�mB	�mB	�B	�B	�B	��B
B
DB
�B
#�B
-B
49B
;dB
B�B
F�B
M�B
R�B
YB
^5B
cTB
iyB
m�B
p�B
s�B
u�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.3 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201101452032012011014520320120110145203  AO  ARGQ                                                                        20111130143759  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130143759  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120110145203  IP                  G�O�G�O�G�O�                