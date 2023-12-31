CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-28T23:01:43Z UW 3.1 conversion   
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901072 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               A   AO  20111130143635  20190522121828  1728_5048_016                   2C  D   APEX                            2142                            040306                          846 @�R��h�	1   @�R�/h@@5|�hr��cbn��O�1   GPS     Primary sampling: mixed [deeper than nominal 990dbar: discrete; nominal 990dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�  @�  A   A   A@  A^ffA�  A�  A���A���A�  A���A�  A�  B   B  B  B  B   B(  B0  B7��B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�33B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�33B�33B�  B���B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B���B�  B�  B���B�  C �C�C  C  C�fC
  C  C�C  C�fC  C  C  C�fC  C  C �C"�C$  C%�fC(  C*�C,�C.�C0�C2�C4�C6�C8�C:  C;�fC>  C@�CB�CD�CF�CH�CJ  CK�fCN  CP  CR�CT�CV  CX  CZ  C\  C^  C`  Cb�Cd  Ce�fCh  Cj  Cl  Cn�Cp�Cr�Ct  Cu�fCw�fCz  C|�C~�C�  C��3C�  C�  C��C�  C�  C�  C��C�  C��3C�  C�  C��C��C��C��C��C��C��C�  C��C�  C�  C�  C�  C��3C�  C��C�  C�  C�  C�  C�  C��3C�  C�  C��3C��3C�  C�  C��C��C��C�  C��3C��3C�  C�  C�  C��C�  C��3C��3C��3C��C�  C�  C��C�  C�  C�  C�  C�  C��C�  C��3C��3C��3C�  C�  C��3C��3C�  C�  C�  C�  C��3C�  C�  C�  C�  C��3C�  C�  C��3C��3C�  C��C��C��C��C��C�  C�  C�  C��C�  C��3C�  C��C�  C�  C�  C��3C�  C��C�  C�  C��3C�  C��C�  C�  C�  C��3C��3C��3C��3C��3C��3C�  C��C��C��C��C��C�  D   D � D ��D� D  Dy�D��Dy�D��D� D  D� D��Dy�D  D� D  D�fD	  D	� D
fD
� D  D� D��D� D  D� D  D� D  D� DfD� D  D� DfD�fDfD�fD  D� D��D� DfD�fDfD�fDfD�fDfD�fD  Dy�D  D� D  D� D��D� D  D� D��D� D   D �fD!fD!�fD"fD"�fD#  D#� D$  D$� D%fD%� D&  D&y�D'  D'� D(  D(y�D)  D)� D*  D*y�D+  D+�fD,  D,� D-  D-y�D-��D.y�D.��D/y�D/��D0� D1fD1� D2  D2� D3  D3� D4  D4y�D5  D5� D5��D6y�D7  D7� D8  D8� D9  D9� D:fD:� D;  D;y�D<  D<� D<��D=� D>  D>y�D>��D?� D@  D@� DA  DAy�DA��DB� DCfDC� DD  DD� DD��DEy�DF  DF� DG  DGy�DH  DH�fDI  DIy�DJ  DJ� DK  DK� DLfDL� DM  DM�fDNfDN�fDO  DOy�DP  DP� DQ  DQ� DRfDR� DS  DS� DT  DT� DU  DU�fDV  DV� DW  DW� DX  DX� DY  DY� DZfDZ� D[  D[�fD\  D\� D]  D]� D]��D^� D_  D_� D`  D`� DafDa� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� DifDi� Dj  Dj� Dk  Dk�fDl  Dl� DmfDm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dry�Ds  Ds� Dt  Dt� Du  Duy�Dv  Dv� Dw  Dy��D���D�C3D�vfD�ɚD�� D�,�D���D��3D�  D�&fD�i�Dǹ�D��3D�9�D�ffD�fD��fD��D�I�D�31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @l��@�ff@�ffA33A;33AY��A{33A���A�ffA�ffA���A�ffAݙ�A홚A���B��B��B��B��B&��B.��B6ffB>��BF��BN��BV��B^��Bf��Bn��Bv��B~��B���B�ffB�ffB�ffB�ffB���B�ffB�ffB�ffB�ffB�ffB�ffB���B���B�ffB�33B�ffB�ffB�ffB�ffB�ffB�ffBۙ�B�ffB�ffB�ffB�33B�ffB�ffB�33B�ffB���C��C�3C�3C��C	�3C�3C��C�3C��C�3C�3C�3C��C�3C�3C��C!��C#�3C%��C'�3C)��C+��C-��C/��C1��C3��C5��C7��C9�3C;��C=�3C?��CA��CC��CE��CG��CI�3CK��CM�3CO�3CQ��CS��CU�3CW�3CY�3C[�3C]�3C_�3Ca��Cc�3Ce��Cg�3Ci�3Ck�3Cm��Co��Cq��Cs�3Cu��Cw��Cy�3C{��C}��C�3C���C�ٚC�ٚC��fC�ٚC�ٚC�ٚC��fC�ٚC���C�ٚC�ٚC��fC��fC��fC��fC��fC��fC��fC�ٚC��fC�ٚC�ٚC�ٚC�ٚC���C�ٚC��fC�ٚC�ٚC�ٚC�ٚC�ٚC���C�ٚC�ٚC���C���C�ٚC�ٚC��fC��fC��fC�ٚC���C���C�ٚC�ٚC�ٚC��fC�ٚC���C���C���C��fC�ٚC�ٚC��fC�ٚC�ٚC�ٚC�ٚC�ٚC��fC�ٚC���C���C���C�ٚC�ٚC���C���C�ٚC�ٚC�ٚC�ٚC���C�ٚC�ٚC�ٚC�ٚC���C�ٚC�ٚC���C���C�ٚC��fC��fC��fC��fC��fC�ٚC�ٚC�ٚC��fC�ٚC���C�ٚC��fC�ٚC�ٚC�ٚC���C�ٚC��fC�ٚC�ٚC���C�ٚC��fC�ٚC�ٚC�ٚC���C���C���C���C���C���C�ٚC��fC��fC��fC��fC��fC�ٚC�ٚD l�D �fDl�D��DffD�fDffD�fDl�D��Dl�D�fDffD��Dl�D��Ds3D��D	l�D	�3D
l�D
��Dl�D�fDl�D��Dl�D��Dl�D��Dl�D�3Dl�D��Dl�D�3Ds3D�3Ds3D��Dl�D�fDl�D�3Ds3D�3Ds3D�3Ds3D�3Ds3D��DffD��Dl�D��Dl�D�fDl�D��Dl�D�fDl�D��D s3D �3D!s3D!�3D"s3D"��D#l�D#��D$l�D$�3D%l�D%��D&ffD&��D'l�D'��D(ffD(��D)l�D)��D*ffD*��D+s3D+��D,l�D,��D-ffD-�fD.ffD.�fD/ffD/�fD0l�D0�3D1l�D1��D2l�D2��D3l�D3��D4ffD4��D5l�D5�fD6ffD6��D7l�D7��D8l�D8��D9l�D9�3D:l�D:��D;ffD;��D<l�D<�fD=l�D=��D>ffD>�fD?l�D?��D@l�D@��DAffDA�fDBl�DB�3DCl�DC��DDl�DD�fDEffDE��DFl�DF��DGffDG��DHs3DH��DIffDI��DJl�DJ��DKl�DK�3DLl�DL��DMs3DM�3DNs3DN��DOffDO��DPl�DP��DQl�DQ�3DRl�DR��DSl�DS��DTl�DT��DUs3DU��DVl�DV��DWl�DW��DXl�DX��DYl�DY�3DZl�DZ��D[s3D[��D\l�D\��D]l�D]�fD^l�D^��D_l�D_��D`l�D`�3Dal�Da��Dbl�Db��Dcl�Dc��Ddl�Dd��Del�De��Dfl�Df��Dgl�Dg��Dhl�Dh�3Dil�Di��Djl�Dj��Dks3Dk��Dll�Dl�3Dml�Dm��Dnl�Dn��Dol�Do��Dpl�Dp��Dql�Dq��DrffDr��Dsl�Ds��Dtl�Dt��DuffDu��Dvl�Dv��Dyy�D�� D�9�D�l�D�� D��fD�#3D�� D���D��fD��D�` Dǰ D�ٚD�0 D�\�D���D���D�3D�@ D�	�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��HA��HA��A��A��A��A��A��A��A��A��A��A���A���A���A���A���A���A���A���A�  A�  A�A�  A�A�A�A�%A�1A�
=A�VA�bA�VA�
=A�1A�JA�VA�{A�oA�%A���A���A��yA��/A���A�ȴA���AļjAĴ9AđhA�Q�A�ĜA�A��A���A�?}A��mA�S�A�t�A�n�A���A��DA�^5A�ȴA���A�Q�A�?}A��HA�ZA��A���A���A�`BA��A��`A��uA��`A��A�9XA���A�bA���A�|�A��;A�1'A��PA�\)A��^A���A�7LA�A��9A�?}A�hsA�t�A���A���A��A�ffA���A�t�A�n�A���A��jA���A���A��/A��9A��A�5?A���A�z�A�`BA�/A��A�5?A�ƨA�n�A�|�A��A��hA�x�A}?}A|VA|JA{;dAy�AvffAsAq��Ao�Am;dAm?}AlffAk%Ai�AhQ�Af�Ae"�AdVAbZA_ƨA^��A]�wA\�AYC�AU�AT�\ASl�AQp�AP �AOXAM�hAKAIXAG�FAF1'AD�ACG�AA��AA;dA@�HA@5?A?�FA?
=A>n�A=�^A<��A;�A9�A7��A6�\A4�A3��A2VA1A0 �A/?}A-�;A+��A*�+A)�wA(��A'��A&�A&E�A$��A#�FA"VA ��AdZA��A��AbNA��AXA��A��A7LA�/A��A33A^5AS�A��A�A��A+A�DA�#A7LA�RA�#A�A1A�A`BA�A�^A�7AK�A�A�;A
�`A	��A	`BA	VA��A�\AI�A�^A��A�;Av�A|�A&�AĜAE�At�A �!A �@�$�@��@�@�D@�@�~�@�V@�`B@�t�@�=q@�&�@��@�
=@�x�@畁@�ff@�h@��@��/@�@�dZ@�{@��@��m@ޗ�@�/@ۥ�@��@���@�$�@�hs@ش9@�1'@׶F@�t�@�;d@�@���@֏\@�V@�=q@�@ղ-@���@�1@�\)@�@��@��@���@��/@��@�Ĝ@У�@�z�@�1@�l�@��@���@Ώ\@�E�@��T@́@̼j@�9X@�1@���@˕�@�\)@�o@ʏ\@���@�X@���@ȓu@�I�@��
@�|�@�t�@�dZ@�;d@��H@�n�@�E�@�5?@�@š�@�?}@��`@Ĵ9@ě�@�A�@��
@å�@�l�@�"�@��y@§�@���@�I�@���@��
@�b@���@���@�b@�V@�@��7@�O�@�7L@��@��^@��@��`@��9@�bN@�1'@�  @��w@��P@��w@��u@��#@�=q@���@��h@�x�@�`B@�O�@��@��u@� �@�ƨ@��H@���@���@��R@�5?@��@���@���@�x�@�%@��@���@��j@���@�(�@��m@�ƨ@�dZ@�o@���@���@�X@�&�@��`@�Z@� �@��m@�ƨ@��@�o@��\@�M�@��@�?}@��@���@�A�@��@��+@�/@��j@��u@��@���@���@�1'@���@��@��+@�@���@��@���@��@��@��D@�Q�@�b@��m@�1@�b@��
@���@���@�K�@��R@��R@�ȴ@���@�=q@���@���@�7L@��`@��@� �@�dZ@��R@�ff@�@���@���@���@��-@�p�@�V@���@��9@�z�@�Q�@��@���@��P@�\)@��H@�M�@��@���@���@��@�x�@�7L@���@��u@�1'@�b@���@���@�t�@�
=@��@���@��R@���@�~�@��@��T@���@�I�@��F@�^5@v��@vE�@i%@^��@X�u@Q�^@K"�@F5?@=�-@6E�@1%@,j@'�P@ Ĝ@1@l�@M�@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��HA��HA��A��A��A��A��A��A��A��A��A��A���A���A���A���A���A���A���A���A�  A�  A�A�  A�A�A�A�%A�1A�
=A�VA�bA�VA�
=A�1A�JA�VA�{A�oA�%A���A���A��yA��/A���A�ȴA���AļjAĴ9AđhA�Q�A�ĜA�A��A���A�?}A��mA�S�A�t�A�n�A���A��DA�^5A�ȴA���A�Q�A�?}A��HA�ZA��A���A���A�`BA��A��`A��uA��`A��A�9XA���A�bA���A�|�A��;A�1'A��PA�\)A��^A���A�7LA�A��9A�?}A�hsA�t�A���A���A��A�ffA���A�t�A�n�A���A��jA���A���A��/A��9A��A�5?A���A�z�A�`BA�/A��A�5?A�ƨA�n�A�|�A��A��hA�x�A}?}A|VA|JA{;dAy�AvffAsAq��Ao�Am;dAm?}AlffAk%Ai�AhQ�Af�Ae"�AdVAbZA_ƨA^��A]�wA\�AYC�AU�AT�\ASl�AQp�AP �AOXAM�hAKAIXAG�FAF1'AD�ACG�AA��AA;dA@�HA@5?A?�FA?
=A>n�A=�^A<��A;�A9�A7��A6�\A4�A3��A2VA1A0 �A/?}A-�;A+��A*�+A)�wA(��A'��A&�A&E�A$��A#�FA"VA ��AdZA��A��AbNA��AXA��A��A7LA�/A��A33A^5AS�A��A�A��A+A�DA�#A7LA�RA�#A�A1A�A`BA�A�^A�7AK�A�A�;A
�`A	��A	`BA	VA��A�\AI�A�^A��A�;Av�A|�A&�AĜAE�At�A �!A �@�$�@��@�@�D@�@�~�@�V@�`B@�t�@�=q@�&�@��@�
=@�x�@畁@�ff@�h@��@��/@�@�dZ@�{@��@��m@ޗ�@�/@ۥ�@��@���@�$�@�hs@ش9@�1'@׶F@�t�@�;d@�@���@֏\@�V@�=q@�@ղ-@���@�1@�\)@�@��@��@���@��/@��@�Ĝ@У�@�z�@�1@�l�@��@���@Ώ\@�E�@��T@́@̼j@�9X@�1@���@˕�@�\)@�o@ʏ\@���@�X@���@ȓu@�I�@��
@�|�@�t�@�dZ@�;d@��H@�n�@�E�@�5?@�@š�@�?}@��`@Ĵ9@ě�@�A�@��
@å�@�l�@�"�@��y@§�@���@�I�@���@��
@�b@���@���@�b@�V@�@��7@�O�@�7L@��@��^@��@��`@��9@�bN@�1'@�  @��w@��P@��w@��u@��#@�=q@���@��h@�x�@�`B@�O�@��@��u@� �@�ƨ@��H@���@���@��R@�5?@��@���@���@�x�@�%@��@���@��j@���@�(�@��m@�ƨ@�dZ@�o@���@���@�X@�&�@��`@�Z@� �@��m@�ƨ@��@�o@��\@�M�@��@�?}@��@���@�A�@��@��+@�/@��j@��u@��@���@���@�1'@���@��@��+@�@���@��@���@��@��@��D@�Q�@�b@��m@�1@�b@��
@���@���@�K�@��R@��R@�ȴ@���@�=q@���@���@�7L@��`@��@� �@�dZ@��R@�ff@�@���@���@���@��-@�p�@�V@���@��9@�z�@�Q�@��@���@��P@�\)@��H@�M�@��@���@���@��@�x�@�7L@���@��u@�1'@�b@���@���@�t�@�
=@��@���@��R@���@�~�@��@��T@���@�I�@��F@�^5@v��@vE�@i%@^��@X�u@Q�^@K"�@F5?@=�-@6E�@1%@,j@'�P@ Ĝ@1@l�@M�@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B!�B"�B%�B+B2-B9XB@�BG�BI�BI�BJ�BL�BM�BP�BS�BVBW
BZBVBiyBl�Bl�Bv�B�7B�JB�oB�bB�{B��B�B�?B�^B�qB�jB�?B�B�B��B��B��B��B��B�\B�=B�B{�Bm�BaHBT�BO�BL�BI�BB�B:^B5?B0!B#�B�B%B  B��B�B�ZB�BɺB�RB��B��B��B�{By�BjB[#B7LB �BuB+B
��B
�B
�B
�yB
�ZB
��B
�FB
��B
|�B
hsB
T�B
9XB
�B	�B	�;B	�B	��B	��B	��B	�oB	�1B	�B	v�B	�=B	�{B	�PB	�+B	~�B	v�B	k�B	cTB	ZB	K�B	H�B	>wB	33B	�B	oB	hB	oB	
=B	B	%B	B	B��B�B�B�mB�TB�/B�B�
B�B��B��B��B��BȴB�}B�LB�?B�'B�?B�9B�B��B��B��B��B�uB��B��B��B��B��B�oB�oB�\B�PB�=B�DB�=B�7B�1B�+B�+B�1B�%B�B�B� B}�B}�B~�B�B�B� B� B� B�B�B�B�B�%B� B}�B|�B~�B�B�B�B�B�B�B�B�B�B�B�B~�B� B}�B� B�+B�1B�VB�\B�VB�PB�DB�DB�+B�B�1B�%B�+B�1B�+B�1B�=B�7B�1B�1B�=B�7B�%B�+B�=B�=B�DB�DB�VB�uB��B��B��B��B��B��B��B�B�!B�9B�LB�^B�}BBÖBĜBƨBŢBŢBĜBŢB��B��B��B�B�B�5B�sB�B�B��B��B��B	  B	B	%B	1B	
=B	VB	oB	�B	�B	 �B	!�B	#�B	%�B	'�B	(�B	,B	/B	49B	7LB	;dB	<jB	@�B	D�B	F�B	H�B	H�B	K�B	N�B	O�B	O�B	P�B	R�B	T�B	W
B	XB	YB	ZB	[#B	\)B	\)B	]/B	\)B	\)B	[#B	\)B	]/B	`BB	dZB	ffB	dZB	]/B	XB	VB	W
B	XB	]/B	dZB	hsB	gmB	ffB	ffB	gmB	hsB	iyB	l�B	r�B	v�B	~�B	�%B	�=B	�JB	�PB	�VB	�VB	�VB	�\B	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�'B	�-B	�3B	�FB	�LB	�LB	�XB	�^B	�jB	�qB	�qB	�wB	�dB	�RB	�^B	�dB	�wB	B	ÖB	ŢB	ƨB	ƨB	ƨB	ƨB	ǮB	ƨB	ƨB	ǮB	ƨB	ŢB	ĜB	ĜB	ŢB	ȴB	��B	��B	��B	��B	��B	��B	��B	�B	�#B	�/B	�/B	�5B	�5B	�5B	�/B	�/B	�5B	�/B	�;B	�TB	�ZB	�`B	�fB	�sB	�sB	�sB	�sB	�yB	�yB	�yB	�yB	�yB	�yB	�yB	�yB	�sB	�yB	�yB	�sB	�mB	�mB	�mB	�mB	�mB	�sB	�yB	�yB	�B	�B	�B	�yB	�B	�B	�B	�yB	�B	�B	�B	�B
B
JB	��B
�B
{B
�B
�B
-B
>wB
E�B
K�B
P�B
YB
`BB
_;B
bNB
dZB
iyB
m�B
s�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B!�B"�B%�B+B2-B9XBA�BG�BI�BJ�BK�BL�BN�BP�BT�BW
B[#BbNBn�B{�Bx�B{�B�+B��B��B��B��B��B��B�-B�qBŢBǮB��BŢB�qB�-B�'B�?B�3B��B��B��B�{B�bB�bB|�BhsBYBT�BT�BQ�BI�B?}B?}B>wB49B$�BDB1BB��B�B�`B�BĜB�-B�B�B��B�1Bz�By�BL�B/B�BbB
��B
��B
�B
�B
�B
�fB
��B
�B
�VB
� B
l�B
W
B
)�B	��B	�ZB	�ZB	�HB	�
B	�qB	��B	��B	�JB	y�B	�oB	��B	��B	�{B	�DB	�B	t�B	r�B	jB	T�B	Q�B	L�B	J�B	,B	�B	�B	�B	{B	PB	{B	oB	hB	
=B��B��B�B�B�NB�)B�)B�B�B�
B��B��B��B��BÖB��B�dB�qB�wB�LB�'B�B�B��B��B��B��B��B��B��B��B��B��B��B�hB�VB�PB�JB�JB�JB�PB�VB�=B�1B�1B�+B�B�B�B�B�B�B�B�%B�1B�7B�JB�JB�DB�B�B�B�B�B�B�%B�7B�JB�DB�+B�%B�B�B�B�B�1B�%B�=B�VB�DB�oB�{B��B�{B�oB��B�{B�uB�hB�DB�7B�=B�JB�\B�\B�VB�PB�PB�hB�bB�=B�+B�JB�JB�DB�DB�VB�uB��B��B��B��B��B��B�B�B�!B�FB�XB�^B��BÖBĜBĜBǮBƨBƨBƨBȴB��B��B��B�B�)B�HB�sB�B�B��B��B��B	  B	%B	%B	1B	
=B	VB	oB	�B	�B	 �B	"�B	#�B	&�B	)�B	+B	/B	/B	6FB	9XB	=qB	<jB	A�B	D�B	F�B	H�B	H�B	K�B	N�B	P�B	P�B	R�B	S�B	T�B	XB	YB	YB	[#B	\)B	]/B	\)B	^5B	^5B	_;B	[#B	^5B	]/B	`BB	ffB	l�B	dZB	]/B	XB	VB	W
B	XB	\)B	dZB	jB	hsB	gmB	gmB	hsB	iyB	iyB	l�B	q�B	v�B	~�B	�%B	�DB	�PB	�VB	�\B	�\B	�VB	�hB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�'B	�3B	�?B	�RB	�RB	�LB	�dB	�dB	�jB	�qB	�qB	B	�dB	�XB	�dB	�dB	�wB	B	ŢB	ǮB	ȴB	ǮB	ȴB	��B	ǮB	ɺB	ȴB	ɺB	ȴB	ƨB	ŢB	ŢB	ŢB	ȴB	��B	��B	��B	��B	��B	��B	��B	�B	�)B	�5B	�5B	�;B	�;B	�5B	�5B	�;B	�BB	�5B	�BB	�ZB	�ZB	�`B	�mB	�yB	�yB	�yB	�yB	�B	�B	�yB	�B	�B	�B	�B	�yB	�yB	�B	�yB	�sB	�mB	�sB	�sB	�sB	�sB	�sB	�yB	�B	�B	�B	�B	�yB	�B	�B	�B	�yB	�B	�B	�B	�B
B
JB	��B
�B
{B
�B
 �B
.B
>wB
E�B
K�B
Q�B
YB
`BB
`BB
bNB
dZB
jB
m�B
s�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<ě�<�t�<D��<u<�o<�o<�C�<#�
<49X<#�
<#�
<#�
<#�
<#�
<#�
<u<�o<e`B<#�
<#�
<T��<T��<#�
<#�
<#�
<#�
<T��<��
<u<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<e`B<�o<D��<#�
<#�
<#�
<#�
<#�
<T��<u<D��<#�
<#�
<u<��
<e`B<�o<�h<�1<e`B<#�
<#�
<#�
<#�
<#�
<#�
<T��<���<��
<���<�C�<�j<�j<�h<��
<#�
<#�
<#�
<�t�<�9X<��
<D��<�o<#�
<#�
<#�
<#�
<#�
<T��<D��<49X<#�
<u<�o<#�
<#�
<e`B<�j<��
<#�
<#�
<T��<#�
<#�
<e`B<e`B<u<D��<49X<#�
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
<D��<49X<49X<#�
<#�
<#�
<#�
<#�
<#�
<49X<D��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<T��<T��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.3 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201101451582012011014515820120110145158  AO  ARGQ                                                                        20111130143635  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130143635  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120110145158  IP                  G�O�G�O�G�O�                