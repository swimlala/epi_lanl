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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901072 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               A   AO  20111130143628  20190522121828  1728_5048_015                   2C  D   APEX                            2142                            040306                          846 @�P.�1   @�P.� @6vȴ9X�cv�+J1   GPS     Primary sampling: mixed [deeper than nominal 990dbar: discrete; nominal 990dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @,��@�  @���A   A   A@  Aa��A���A�33A�  A�  A�  A���A�  A�  B   B  B��B  B ffB(  B0  B8  B@  BH  BP  BXffB`  Bg��Bp  Bx  B�  B�  B�  B���B�  B�  B�  B�  B�  B���B���B���B���B�  B�  B�  B�  B�33B�  B�33B�33B�  B���B�  B�33B�  B�  B���B�  B�33B�33B�33C �C  C  C  C  C	�fC  C  C  C  C  C�C  C�fC  C�C   C"  C$�C&  C(  C*�C,  C-�fC0  C2  C4  C6  C8  C:  C<  C>�C@  CB  CD  CF�CH  CJ  CL  CN  CO�fCQ�fCT�CV  CW�fCZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Co�fCr  Ct  Cu�fCx  Cz�C|�C~  C��C��C�  C�  C�  C�  C�  C��3C�  C��C��C�  C�  C�  C��3C��3C��3C�  C�  C�  C�  C��C�  C��3C�  C�  C�  C�  C�  C��C��C��C��C�  C��3C��3C��3C�  C��C��C�  C�  C�  C�  C�  C�  C��3C�  C�  C��3C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C��C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��3C�  C�  C��3C��3C�  C�  C�  C��3C�  C��C�  C�  C��3C��3C�  C��C��C��C��C��C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C��C�  C�  C�  C�  C��C�  C��3C�  C�  C�  C��3D � DfD� D  Dy�D  D�fD  D� D��D� DfD�fD  D� D  D� D	  D	�fD
  D
� D  D� D��D� DfD� D  D� D  D� D  D� D��D� DfD� D  D� D  Dy�D  D�fD  Dy�D  D� D  D� D  Dy�D  D�fD  D� D��Dy�D��Dy�D��D� DfD�fD fD �fD!fD!�fD"fD"�fD#  D#y�D#��D$y�D$��D%y�D%��D&y�D&��D'� D(fD(� D)  D)� D*fD*� D*��D+� D,fD,�fD-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3fD3�fD4fD4�fD5fD5� D5��D6y�D7  D7�fD8  D8� D9fD9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>�fD?  D?� D@  D@y�DA  DA� DB  DB�fDC  DC� DDfDD� DE  DE�fDF  DF� DF��DGy�DG��DHy�DI  DI�fDJfDJ� DK  DK� DL  DLy�DL��DM� DNfDN�fDO  DO� DO��DP� DQfDQ�fDRfDR� DS  DSy�DT  DT� DUfDU� DV  DV� DWfDW� DX  DXy�DX��DY� DZ  DZ� DZ��D[� D\  D\y�D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� De��Df� DgfDg� Dh  Dh� Dh��Di� DjfDj� Dk  Dk� Dl  Dl� Dm  Dmy�Dm��Dn� DofDo� Dp  Dp� Dq  Dq�fDr  Dr� Ds  Ds� Ds��Dt� Du  Du� Dv  Dv� DyffD� D�9�D�|�D���D��3D�,�D�s3D���D��3D��D�� Dǹ�D��fD�6fDڐ D�3D���D��D�Y�D�,�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@l��@�33@�ffA33A;33A\��A|��A���A���A���A���A�ffAݙ�A홚A���B��BffB��B33B&��B.��B6��B>��BF��BN��BW33B^��BfffBn��Bv��B~��B�ffB�ffB�33B�ffB�ffB�ffB�ffB�ffB�33B�33B�33B�33B�ffB�ffB�ffB�ffBÙ�B�ffB˙�Bϙ�B�ffB�33B�ffBߙ�B�ffB�ffB�33B�ffB�B���B���B���C�3C�3C�3C�3C	��C�3C�3C�3C�3C�3C��C�3C��C�3C��C�3C!�3C#��C%�3C'�3C)��C+�3C-��C/�3C1�3C3�3C5�3C7�3C9�3C;�3C=��C?�3CA�3CC�3CE��CG�3CI�3CK�3CM�3CO��CQ��CS��CU�3CW��CY�3C[�3C]�3C_�3Ca�3Cc�3Ce�3Cg�3Ci�3Ck�3Cm�3Co��Cq�3Cs�3Cu��Cw�3Cy��C{��C}�3C��C��fC�ٚC�ٚC�ٚC�ٚC�ٚC���C�ٚC��fC��fC�ٚC�ٚC�ٚC���C���C���C�ٚC�ٚC�ٚC�ٚC��fC�ٚC���C�ٚC�ٚC�ٚC�ٚC�ٚC��fC��fC��fC��fC�ٚC���C���C���C�ٚC��fC��fC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC���C�ٚC�ٚC���C�ٚC�ٚC���C���C�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC��fC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC��fC���C�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC��fC�ٚC���C�ٚC�ٚC���C���C�ٚC�ٚC�ٚC���C�ٚC��fC�ٚC�ٚC���C���C�ٚC��fC��fC��fC��fC��fC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC���C�ٚC�ٚC�ٚC��fC�ٚC�ٚC�ٚC�ٚC��fC�ٚC���C�ٚC�ٚC�ٚC���D l�D �3Dl�D��DffD��Ds3D��Dl�D�fDl�D�3Ds3D��Dl�D��Dl�D��D	s3D	��D
l�D
��Dl�D�fDl�D�3Dl�D��Dl�D��Dl�D��Dl�D�fDl�D�3Dl�D��Dl�D��DffD��Ds3D��DffD��Dl�D��Dl�D��DffD��Ds3D��Dl�D�fDffD�fDffD�fDl�D�3Ds3D�3D s3D �3D!s3D!�3D"s3D"��D#ffD#�fD$ffD$�fD%ffD%�fD&ffD&�fD'l�D'�3D(l�D(��D)l�D)�3D*l�D*�fD+l�D+�3D,s3D,��D-l�D-��D.l�D.��D/l�D/��D0l�D0��D1l�D1��D2l�D2�3D3s3D3�3D4s3D4�3D5l�D5�fD6ffD6��D7s3D7��D8l�D8�3D9l�D9��D:l�D:��D;l�D;��D<l�D<��D=l�D=��D>s3D>��D?l�D?��D@ffD@��DAl�DA��DBs3DB��DCl�DC�3DDl�DD��DEs3DE��DFl�DF�fDGffDG�fDHffDH��DIs3DI�3DJl�DJ��DKl�DK��DLffDL�fDMl�DM�3DNs3DN��DOl�DO�fDPl�DP�3DQs3DQ�3DRl�DR��DSffDS��DTl�DT�3DUl�DU��DVl�DV�3DWl�DW��DXffDX�fDYl�DY��DZl�DZ�fD[l�D[��D\ffD\��D]l�D]��D^l�D^��D_l�D_��D`l�D`��Dal�Da��Dbl�Db��Dcl�Dc��Ddl�Dd��Del�De�fDfl�Df�3Dgl�Dg��Dhl�Dh�fDil�Di�3Djl�Dj��Dkl�Dk��Dll�Dl��DmffDm�fDnl�Dn�3Dol�Do��Dpl�Dp��Dqs3Dq��Drl�Dr��Dsl�Ds�fDtl�Dt��Dul�Du��Dvl�DyS3D�fD�0 D�s3D�� D���D�#3D�i�D��3D��D� D��fDǰ D���D�,�DچfDਗ਼D��3D�3D�P D�#31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A��A� �A�"�A�"�A�$�A�$�A�$�A�$�A�$�A�&�A�&�A�(�A�(�A�(�A�+A�+A�+A�-A�-A�/A�/A�/A�/A�1'A�1'A�1'A�33A�5?A�7LA�9XA�7LA�7LA�9XA�5?A�5?A�5?A�5?A�7LA�;dA�=qA�?}A�=qA�;dA�9XA�/A�bA�A���A�bNA�?}A�M�A�JA��uA�-A��\A��`A��A�$�A�v�A�\)A�n�A�VA�M�A���A�A�9XA�M�A�S�A�1'A��A��TA�VA�|�A���A�-A��wA���A��7A���A�{A�hsA�bA�$�A�|�A�M�A�ȴA�7LA��A���A��`A�;dA���A��HA�XA��yA���A�M�A���A�ffA�bNA�  A���A��wA�jA�XA�M�A�;dA��9A���A��wA��A�1'A��-A��A��hA���A�;A~��A|��Ay?}AwO�Av�/Avn�Au7LAsx�Arz�Aq�^AodZAmC�Ak��Aj��AhAc�wAa��AaG�A`r�A_��A_+A^�HA^E�A[�wAW�
AV �AU�AS�^AR��ARAP��AOl�ANZAM+AK��AJ�yAI7LAGC�AE��AE&�AD�RAD9XAC%AAx�A?�wA>�\A;��A:�A8��A7
=A5��A3dZA1x�A/K�A,�!A,VA+��A+t�A*��A*  A)x�A)�A(ȴA(�DA(ZA(A'�wA'��A'�PA'l�A&�A%�;A%|�A%�A$bNA"�HA!��A n�AK�AĜA�A�A�!A��AffA��AG�AZAp�A�A�HAjA��A�/AbNA{A�PA��A�At�At�A5?A�HA��A	�;A�`Az�A(�A�A�^A%A�hAx�Ar�A��A ��@�\)@��h@�1'@��R@�I�@���@�I�@�|�@�@�`B@���@�  @�P@��@��y@�ȴ@�\@�@��@ꟾ@�j@���@�{@��`@�t�@�!@��#@�z�@ߕ�@އ+@ݑh@�7L@�33@�J@���@٩�@ّh@�x�@�G�@��@�%@ش9@�r�@�  @�l�@���@�n�@���@��@�z�@��
@�C�@�E�@ёh@�/@��/@У�@�A�@�dZ@Χ�@�@́@��@���@�hs@��@Ǿw@���@�=q@�V@�z�@�1@�33@�{@���@��@���@��@�$�@���@���@�hs@��j@���@�C�@��@�~�@�-@���@���@�hs@��@���@��@�Ĝ@��/@�V@��7@��@���@�ȴ@�S�@�33@��@���@�$�@�@�/@��/@���@�Ĝ@��@�Q�@�1'@��@�b@�1@��m@�|�@�ȴ@�$�@��-@�G�@�V@���@�Ĝ@�j@��@��;@���@�dZ@�;d@�
=@��\@�@��-@�G�@��@��@��;@�K�@�
=@���@���@�z�@�"�@��!@�J@�7L@�j@�\)@��@���@�~�@�n�@�5?@���@�p�@�&�@�V@��@�Ĝ@���@�r�@� �@��m@��
@��F@�33@�v�@�J@��#@��-@��7@�G�@���@��@�1'@��m@���@�K�@��@���@�v�@��@��#@��7@�G�@�G�@�`B@�p�@�?}@��`@�  @�t�@�K�@�33@�+@�
=@�@��@��+@�5?@��@���@��@�G�@��@���@��/@��@��@�Q�@��@���@�l�@�C�@�+@�o@��y@���@���@�n�@�V@��@��@���@��@�O�@���@���@��u@�I�@�  @���@���@���@��P@�\)@�@��!@�-@��#@���@��7@�p�@�O�@�7L@�7L@�%@���@�Ĝ@��u@�z�@�Q�@�1@��F@�S�@���@��-@�Q�@�M�@��!@v��@kC�@b~�@RM�@KdZ@E�@@Ĝ@;C�@49X@.ȴ@)G�@#t�@�
@A�@V@�@�
1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��A��A� �A�"�A�"�A�$�A�$�A�$�A�$�A�$�A�&�A�&�A�(�A�(�A�(�A�+A�+A�+A�-A�-A�/A�/A�/A�/A�1'A�1'A�1'A�33A�5?A�7LA�9XA�7LA�7LA�9XA�5?A�5?A�5?A�5?A�7LA�;dA�=qA�?}A�=qA�;dA�9XA�/A�bA�A���A�bNA�?}A�M�A�JA��uA�-A��\A��`A��A�$�A�v�A�\)A�n�A�VA�M�A���A�A�9XA�M�A�S�A�1'A��A��TA�VA�|�A���A�-A��wA���A��7A���A�{A�hsA�bA�$�A�|�A�M�A�ȴA�7LA��A���A��`A�;dA���A��HA�XA��yA���A�M�A���A�ffA�bNA�  A���A��wA�jA�XA�M�A�;dA��9A���A��wA��A�1'A��-A��A��hA���A�;A~��A|��Ay?}AwO�Av�/Avn�Au7LAsx�Arz�Aq�^AodZAmC�Ak��Aj��AhAc�wAa��AaG�A`r�A_��A_+A^�HA^E�A[�wAW�
AV �AU�AS�^AR��ARAP��AOl�ANZAM+AK��AJ�yAI7LAGC�AE��AE&�AD�RAD9XAC%AAx�A?�wA>�\A;��A:�A8��A7
=A5��A3dZA1x�A/K�A,�!A,VA+��A+t�A*��A*  A)x�A)�A(ȴA(�DA(ZA(A'�wA'��A'�PA'l�A&�A%�;A%|�A%�A$bNA"�HA!��A n�AK�AĜA�A�A�!A��AffA��AG�AZAp�A�A�HAjA��A�/AbNA{A�PA��A�At�At�A5?A�HA��A	�;A�`Az�A(�A�A�^A%A�hAx�Ar�A��A ��@�\)@��h@�1'@��R@�I�@���@�I�@�|�@�@�`B@���@�  @�P@��@��y@�ȴ@�\@�@��@ꟾ@�j@���@�{@��`@�t�@�!@��#@�z�@ߕ�@އ+@ݑh@�7L@�33@�J@���@٩�@ّh@�x�@�G�@��@�%@ش9@�r�@�  @�l�@���@�n�@���@��@�z�@��
@�C�@�E�@ёh@�/@��/@У�@�A�@�dZ@Χ�@�@́@��@���@�hs@��@Ǿw@���@�=q@�V@�z�@�1@�33@�{@���@��@���@��@�$�@���@���@�hs@��j@���@�C�@��@�~�@�-@���@���@�hs@��@���@��@�Ĝ@��/@�V@��7@��@���@�ȴ@�S�@�33@��@���@�$�@�@�/@��/@���@�Ĝ@��@�Q�@�1'@��@�b@�1@��m@�|�@�ȴ@�$�@��-@�G�@�V@���@�Ĝ@�j@��@��;@���@�dZ@�;d@�
=@��\@�@��-@�G�@��@��@��;@�K�@�
=@���@���@�z�@�"�@��!@�J@�7L@�j@�\)@��@���@�~�@�n�@�5?@���@�p�@�&�@�V@��@�Ĝ@���@�r�@� �@��m@��
@��F@�33@�v�@�J@��#@��-@��7@�G�@���@��@�1'@��m@���@�K�@��@���@�v�@��@��#@��7@�G�@�G�@�`B@�p�@�?}@��`@�  @�t�@�K�@�33@�+@�
=@�@��@��+@�5?@��@���@��@�G�@��@���@��/@��@��@�Q�@��@���@�l�@�C�@�+@�o@��y@���@���@�n�@�V@��@��@���@��@�O�@���@���@��u@�I�@�  @���@���@���@��P@�\)@�@��!@�-@��#@���@��7@�p�@�O�@�7L@�7L@�%@���@�Ĝ@��u@�z�@�Q�@�1@��F@�S�@���@��-@�Q�@�M�@��!@v��@kC�@b~�@RM�@KdZ@E�@@Ĝ@;C�@49X@.ȴ@)G�@#t�@�
@A�@V@�@�
1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBoBoBoBoBhBoBoBoBoBoBoBoBoBoBoBoBoBoBoBoBoBoBoBhBhBhBhBhBhBoBhBhBhBoBoBoBoBoBoBoBoBoBoBoBhBhBbBVB
=B%BBuB>wBF�BYBhsBk�B� B�{B��B��B��B�\B�oB��B��B��B��B�;B�5B�;B�HB�;B�B��BB�^B��B�VB�By�Bl�BdZBT�B1'B�BuB
=B  B�B�;B��B��BȴB��B��B��BȴBǮB�XB��B�VBffB>wB8RB6FB5?B1'B&�BhB
�mB
��B
�'B
�{B
iyB
B�B
6FB
/B
%�B
�B
  B	��B	�B	�B	�`B	�B	��B	ȴB	�jB	�!B	��B	��B	�PB	x�B	p�B	l�B	hsB	dZB	_;B	[#B	T�B	J�B	1'B	$�B	�B	�B	hB	DB	1B	%B	B	B	  B��B��B�B�B�B��B�B�B�B�ZB�5B��BĜB�^B�!B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B��B��B��B�uB�{B�bB�\B�hB�\B�\B�bB�\B�\B�\B�bB�uB�oB�bB�VB�PB�JB�JB�hB�oB�hB�oB�\B�JB�JB�1B�=B�7B�B�B�B�B�B�B~�B� B� B� B� B~�B� B� B�B� B|�B� B�B�B�B�B�%B�+B�1B�7B�7B�=B�=B�7B�1B�+B�7B�DB�\B�VB�PB�JB�JB�\B�oB��B��B��B��B��B��B��B�B�B�B�3B�qB��BǮB��B��B��B�B�;B�TB�sB�B�B�B�B��B��B��B��B��B	B	%B	
=B	DB	DB	PB	
=B	+B	+B	1B	PB	bB	VB	JB		7B		7B	DB	DB	DB	PB	VB	VB	VB	\B	oB	uB	{B	�B	�B	�B	�B	�B	�B	 �B	%�B	'�B	+B	/B	2-B	:^B	L�B	_;B	m�B	v�B	y�B	|�B	}�B	�B	�B	�7B	�DB	�DB	�DB	�PB	�VB	�VB	�VB	�\B	�VB	�\B	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�!B	�'B	�'B	�'B	�-B	�3B	�9B	�9B	�FB	�LB	�RB	�XB	�^B	�dB	�jB	�wB	�}B	��B	��B	ÖB	ĜB	ĜB	ŢB	ŢB	ŢB	ŢB	ƨB	ƨB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�
B	�B	�B	�B	�B	�#B	�#B	�#B	�)B	�/B	�5B	�5B	�5B	�5B	�;B	�HB	�HB	�TB	�ZB	�fB	�mB	�mB	�sB	�yB	�B	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�yB	�sB	��B
B

=B
�B
%�B
,B
-B
5?B
:^B
>wB
F�B
L�B
S�B
YB
`BB
ffB
gmB
l�B
n�B
s�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  BoBoBoBoBhBoBoBoBoBoBoBoBoBoBoBoBoBoBoBoBoBoBoBhBhBhBhBhBhBoBhBhBhBoBoBoBoBoBoBoBoBoBoBoBoBoBhBhB{B�B$�B7LBG�BXBjBy�B{�B�VB��B�B�3B��B��B��B��B��B��B��B�HB�BB�ZB�yB�B�NB��B��BǮB�?B��B�=B�Bt�Bs�BgmB>wB%�B�BuBPB��B�sB�B��B��B��B��B��B�)B�#BɺB�RB��B{�BB�B9XB8RB:^B<jB>wB+B
��B
�B
��B
�?B
�%B
M�B
@�B
9XB
7LB
.B

=B	��B	��B	��B	�B	�BB	�B	�B	��B	�dB	�!B	�'B	��B	�B	u�B	r�B	n�B	hsB	dZB	e`B	jB	bNB	=qB	.B	&�B	�B	�B	�B	oB	bB	PB	PB	
=B	%B	B��B��B��B��B��B��B��B�B�B�B��BȴB�qB�RB�!B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�uB�bB�oB�uB��B��B��B�uB�hB�hB�hB�oB��B��B��B��B��B��B��B�oB�{B�uB�bB�+B�%B�%B�B�B�+B�JB�VB�1B�+B�1B�1B�+B�7B�+B�+B�1B�+B�+B�1B�1B�7B�=B�=B�DB�=B�DB�JB�PB�VB�\B�hB�oB�uB�uB�PB�bB�bB�\B��B��B��B��B��B��B��B��B�B�B�B�9B�wB��BɺB��B��B�
B�B�NB�TB�B�B�B�B�B��B��B��B	B��B	+B		7B	JB	VB	DB	�B	
=B		7B	+B	DB	hB	oB	bB	bB		7B	PB	DB	PB	VB	PB	VB	VB	VB	\B	�B	uB	{B	�B	�B	�B	�B	�B	�B	"�B	%�B	'�B	+B	/B	2-B	:^B	L�B	_;B	l�B	v�B	z�B	}�B	� B	�B	�+B	�7B	�DB	�DB	�DB	�VB	�\B	�\B	�VB	�bB	�\B	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�'B	�'B	�-B	�'B	�'B	�3B	�3B	�9B	�?B	�LB	�LB	�XB	�XB	�^B	�dB	�qB	�wB	�}B	��B	B	ÖB	ǮB	ƨB	ƨB	ŢB	ŢB	ƨB	ƨB	ƨB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�
B	�B	�B	�B	�B	�#B	�)B	�)B	�#B	�/B	�/B	�5B	�5B	�5B	�;B	�BB	�NB	�HB	�TB	�`B	�fB	�mB	�mB	�sB	�yB	�B	�B	�yB	�B	�B	�B	�B	�B	�B	�B	�yB	�sB	��B
B

=B
�B
%�B
,B
.B
5?B
:^B
>wB
F�B
M�B
S�B
YB
`BB
ffB
gmB
l�B
n�B
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
<�9X<��=\)<#�
<�o<�C�<�C�<�o<e`B<#�
<e`B<��
<u<49X<e`B<49X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<#�
<#�
<#�
<T��<e`B<49X<#�
<#�
<#�
<u<�t�<T��<#�
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
<���<���<�o<��
<���<�1<#�
<#�
<#�
<#�
<49X<�9X<���<�C�<���=C�<��<�`B<49X<#�
<#�
<�C�<��
<#�
<#�
<#�
<#�
<49X<#�
<#�
<u<e`B<49X<D��<��
<�j<49X<#�
<#�
<#�
<#�
<#�
<#�
<��
<�j<49X<#�
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
<49X<e`B<T��<D��<�t�<49X<49X<T��<T��<�o<e`B<�o<T��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<D��<�t�<T��<#�
<#�
<#�
<D��<#�
<#�
<#�
<#�
<#�
<#�
<49X<e`B<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.3 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201101451572012011014515720120110145158  AO  ARGQ                                                                        20111130143628  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130143628  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120110145158  IP                  G�O�G�O�G�O�                