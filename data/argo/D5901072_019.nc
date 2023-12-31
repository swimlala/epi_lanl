CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-28T23:01:44Z UW 3.1 conversion   
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
_FillValue        G�O�   axis      Z          9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�       C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�       M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       i�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       q�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   z   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       |   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �$   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �T   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �T   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �T   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �T   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    � Argo profile    3.1 1.2 19500101000000  5901072 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               A   AO  20111130143654  20190522121828  1728_5048_019                   2C  D   APEX                            2142                            040306                          846 @�Z�i 1   @�Z��?�@4���v��c=����1   GPS     Primary sampling: mixed [deeper than nominal 990dbar: discrete; nominal 990dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @9��@�  @�  A   A   A@  A^ffA�  A�  A�  A�  A�  A�  A�33A�33A�33B  BffBffB ffB(  B/��B8  B@ffBH  BP  BX  B`  Bh  Bp  BxffB�  B�  B�33B�33B�  B���B�  B�33B�33B�33B�33B�  B�  B�  B�33B�33B�33B�33B�  B���B���B���B�  B�33B�  B�  B�  B���B�  B�  B�  B�  C   C  C  C  C�fC	�fC  C  C  C�C  C  C  C  C�C�C   C"�C$  C&  C(  C*  C,�C.  C0  C2  C3�fC5�fC8  C:  C<  C>  C?�fCB  CD  CE�fCH  CJ  CL  CM�fCP  CR�CT�CV  CW�fCZ  C\�C^�C`�Cb  Cd  Cf  Cg�fCi�fCl  Cn�Cp  Cq�fCt  Cv  Cx  Cy�fC{�fC~  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C��3C��3C�  C��C�  C�  C��3C��3C��3C�  C��C�  C��3C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C��C�  C��3C�  C��C�  C�  C��C��3C�  C�  C�  C��3C��3C��3C��3C��3C�  C��C�  C��3C�  C��C�  C�  C�  C��3C�  C��C�  C��3C��3C�  C��C�  C�  C��3C��3C��3C��3C�  C��C��C��C��C�  C��C��C��C��C��C��C��C�  C��3C��3C��3C��3C��3C��3C��3C��3C��3C�  C��C�  C�  C�  C��C�  C��3C�  C�  C�  C��3C�  C�  C�  C��C�  C�  C��C�  C��3C�  C�  C�  C�  C��3C�  D   D � D  D� D  D� DfD� D��Dy�D��Dy�D��D� D  D� D  D�fD	  D	y�D
  D
�fD  D� DfD� D��D� D  D� DfD� D  D� D��Dy�D  D� D  D� D  D� DfD�fDfD�fDfD� D��Dy�D��D� D  D� D  D� DfD� D��D� D  D� D��D� D   D � D!fD!� D"  D"�fD#  D#� D$  D$�fD%  D%� D&  D&� D'  D'� D(  D(�fD)  D)y�D)��D*� D+  D+� D,  D,y�D-  D-� D.  D.�fD/fD/�fD0  D0y�D0��D1� D2  D2� D3  D3� D3��D4� D5  D5� D6  D6� D7  D7� D8fD8� D9  D9� D:  D:� D:��D;� D<  D<y�D=  D=� D>  D>� D>��D?� D@  D@y�DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DG��DH� DI  DI� DJ  DJy�DK  DK� DL  DL� DM  DM� DN  DN�fDOfDO�fDP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DY��DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dcy�Dc��Dd� De  De� De��Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dwl�Dy��D��fD�9�D�y�D���D���D�0 D�vfD��3D���D��D�ffDǹ�D��fD�)�DچfD๚D���D�3D�L�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @&ff@l��@�ff@�ffA33A;33AY��A{33A���A���A���A���A͙�A���A���A���B��B33B33B33B&��B.ffB6��B?33BF��BN��BV��B^��Bf��Bn��Bw33B~��B�ffB���B���B�ffB�33B�ffB���B���B���B���B�ffB�ffB�ffB���B���B���BÙ�B�ffB�33B�33B�33B�ffBۙ�B�ffB�ffB�ffB�33B�ffB�ffB�ffB�ffB�ffC�3C�3C�3C��C	��C�3C�3C�3C��C�3C�3C�3C�3C��C��C�3C!��C#�3C%�3C'�3C)�3C+��C-�3C/�3C1�3C3��C5��C7�3C9�3C;�3C=�3C?��CA�3CC�3CE��CG�3CI�3CK�3CM��CO�3CQ��CS��CU�3CW��CY�3C[��C]��C_��Ca�3Cc�3Ce�3Cg��Ci��Ck�3Cm��Co�3Cq��Cs�3Cu�3Cw�3Cy��C{��C}�3C�3C�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC��fC��fC�ٚC�ٚC���C���C�ٚC��fC�ٚC�ٚC���C���C���C�ٚC��fC�ٚC���C�ٚC�ٚC�ٚC���C�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC���C�ٚC�ٚC�ٚC��fC�ٚC���C�ٚC��fC�ٚC�ٚC��fC���C�ٚC�ٚC�ٚC���C���C���C���C���C�ٚC��fC�ٚC���C�ٚC��fC�ٚC�ٚC�ٚC���C�ٚC��fC�ٚC���C���C�ٚC��fC�ٚC�ٚC���C���C���C���C�ٚC��fC��fC��fC��fC�ٚC��fC��fC��fC��fC��fC��fC��fC�ٚC���C���C���C���C���C���C���C���C���C�ٚC��fC�ٚC�ٚC�ٚC��fC�ٚC���C�ٚC�ٚC�ٚC���C�ٚC�ٚC�ٚC��fC�ٚC�ٚC��fC�ٚC���C�ٚC�ٚC�ٚC�ٚC���C�ٚC�ٚD l�D ��Dl�D��Dl�D�3Dl�D�fDffD�fDffD�fDl�D��Dl�D��Ds3D��D	ffD	��D
s3D
��Dl�D�3Dl�D�fDl�D��Dl�D�3Dl�D��Dl�D�fDffD��Dl�D��Dl�D��Dl�D�3Ds3D�3Ds3D�3Dl�D�fDffD�fDl�D��Dl�D��Dl�D�3Dl�D�fDl�D��Dl�D�fDl�D��D l�D �3D!l�D!��D"s3D"��D#l�D#��D$s3D$��D%l�D%��D&l�D&��D'l�D'��D(s3D(��D)ffD)�fD*l�D*��D+l�D+��D,ffD,��D-l�D-��D.s3D.�3D/s3D/��D0ffD0�fD1l�D1��D2l�D2��D3l�D3�fD4l�D4��D5l�D5��D6l�D6��D7l�D7�3D8l�D8��D9l�D9��D:l�D:�fD;l�D;��D<ffD<��D=l�D=��D>l�D>�fD?l�D?��D@ffD@��DAl�DA��DBl�DB��DCl�DC��DDl�DD��DEl�DE��DFl�DF��DGl�DG�fDHl�DH��DIl�DI��DJffDJ��DKl�DK��DLl�DL��DMl�DM��DNs3DN�3DOs3DO��DPl�DP��DQl�DQ��DRl�DR��DSl�DS��DTl�DT��DUl�DU��DVl�DV��DWl�DW��DXl�DX��DYl�DY�fDZl�DZ��D[l�D[��D\l�D\��D]l�D]��D^l�D^��D_l�D_��D`l�D`��Dal�Da��Dbl�Db��DcffDc�fDdl�Dd��Del�De�fDfl�Df��Dgl�Dg��Dhl�Dh��Dil�Di��Djl�Dj��Dkl�Dk��Dll�Dl��Dml�Dm��Dnl�Dn��Dol�Do��Dpl�Dp��Dql�Dq��Drl�Dr��Dsl�Ds��Dtl�Dt��Dul�Du��Dvl�Dv��DwY�Dy�fD���D�0 D�p D�� D��3D�&fD�l�D���D��3D�3D�\�Dǰ D���D�  D�|�D� D��3D���D�C311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A���A���A��^A��jA��RA��FA��FA��FA��9A��!A���A��A�A�~�A��A�+A��hA�I�A���A��A�z�A��A�/A�7LA�;dA�9XA�9XA�9XA�9XA�5?A�5?A�7LA�9XA�A�A�O�A�jA�r�A�t�A�~�A��A��A�|�A�z�A�z�A�v�A�v�A�|�A�~�A�~�A�x�A�n�A�ZA���A�+A��`A��A�t�A��;A���A�dZA�?}A��
A��A�z�A���A��A�r�A��A���A��A�A�A��A���A�VA��A�bA���A��A�Q�A��!A�33A��HA�ĜA�M�A��A�S�A��\A���A��wA�r�A�VA��`A���A�bA�ȴA���A�A�7LA�-A��!A�7LA��PA��
A��A�jA�v�A��PA��A��yA�7LA���A��\A�r�A�(�A�v�A�O�A�p�A�A�G�A��DA���A|��AxĜAtE�An�9Ai\)Af^5Ae?}Ac��Ab�HAaƨA_A[�AYƨAWl�AT�jAS33AR  AP�jANA�ALAJ�AH�HAH  AGl�AF�AE��AC��AC�AB��AA�wA@Q�A>=qA<1'A:�HA9dZA7��A6�9A6z�A5�TA4��A4�A2�A0A�A-l�A,I�A*�A)ƨA)%A(5?A'`BA'%A&�DA%��A#��A"Q�A!��A!A�
A"�A��AAAK�A�`AJA?}Az�AXA�A�A`BAbA�-A?}AjA+Av�A�FA�yA9XA;dA
1A	t�Ar�A�FA�hAXA�\A��A&�A��@���@���@��@�/@�z�@��@��@��@�@�A�@��y@�hs@�@�+@�@�x�@�F@��@�@�@���@���@�?}@��@�  @�\)@�M�@�X@�r�@��;@ۮ@ڸR@ٲ-@��/@�l�@��@�b@�G�@���@�5?@�@ف@�X@�%@ؓu@�(�@��m@ׅ@��@�V@�X@� �@Ӯ@�o@��#@�bN@ύP@�|�@�;d@��@�Z@�  @̼j@�{@�n�@Ο�@�-@��@���@�j@�dZ@��@�v�@�@��`@�1'@���@�;d@Ƈ+@��@�/@�K�@�{@��/@��F@���@���@��h@��h@���@���@���@��h@�/@�9X@��@���@��H@�l�@��F@���@�K�@�
=@��!@��@�`B@�bN@�b@��;@��
@�dZ@��R@�ff@�-@��@�hs@���@�r�@��@���@��P@�;d@�
=@��@���@���@��+@���@�p�@��@���@���@��D@�1@�ƨ@��P@�dZ@�"�@���@�-@�J@���@��@��j@�j@�Z@�Z@�I�@�  @��@�t�@�;d@�
=@��\@�$�@���@�7L@�V@��@���@�%@���@��@��/@��D@� �@� �@�Z@�1'@���@���@��P@�dZ@�;d@���@���@��\@�n�@�-@��-@�G�@�&�@���@��@�Q�@�ƨ@�o@��\@�V@�-@�{@�@���@�O�@��/@�r�@�(�@�1@��
@��P@��@���@�-@���@���@�`B@�V@�Ĝ@���@�r�@�A�@��@���@�o@��R@��\@�~�@�~�@�n�@�M�@�-@��@���@���@�?}@��`@�Ĝ@���@�Q�@��;@�dZ@�o@���@�$�@�@���@�`B@�%@���@�Z@�9X@�(�@��@�  @��
@���@�C�@��@�~�@�@�X@��@��@���@���@�;d@�
=@��@���@�ff@�@���@�O�@�V@���@��`@���@��j@���@��D@�z�@�bN@�I�@� �@��
@���@��@�1@��/@�G�@�7L@���@�C�@�
=@}�@r��@i��@b�@[t�@SdZ@O;d@H��@A��@:^5@2^5@-��@(  @"�H@V@�u@��@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A���A���A���A��^A��jA��RA��FA��FA��FA��9A��!A���A��A�A�~�A��A�+A��hA�I�A���A��A�z�A��A�/A�7LA�;dA�9XA�9XA�9XA�9XA�5?A�5?A�7LA�9XA�A�A�O�A�jA�r�A�t�A�~�A��A��A�|�A�z�A�z�A�v�A�v�A�|�A�~�A�~�A�x�A�n�A�ZA���A�+A��`A��A�t�A��;A���A�dZA�?}A��
A��A�z�A���A��A�r�A��A���A��A�A�A��A���A�VA��A�bA���A��A�Q�A��!A�33A��HA�ĜA�M�A��A�S�A��\A���A��wA�r�A�VA��`A���A�bA�ȴA���A�A�7LA�-A��!A�7LA��PA��
A��A�jA�v�A��PA��A��yA�7LA���A��\A�r�A�(�A�v�A�O�A�p�A�A�G�A��DA���A|��AxĜAtE�An�9Ai\)Af^5Ae?}Ac��Ab�HAaƨA_A[�AYƨAWl�AT�jAS33AR  AP�jANA�ALAJ�AH�HAH  AGl�AF�AE��AC��AC�AB��AA�wA@Q�A>=qA<1'A:�HA9dZA7��A6�9A6z�A5�TA4��A4�A2�A0A�A-l�A,I�A*�A)ƨA)%A(5?A'`BA'%A&�DA%��A#��A"Q�A!��A!A�
A"�A��AAAK�A�`AJA?}Az�AXA�A�A`BAbA�-A?}AjA+Av�A�FA�yA9XA;dA
1A	t�Ar�A�FA�hAXA�\A��A&�A��@���@���@��@�/@�z�@��@��@��@�@�A�@��y@�hs@�@�+@�@�x�@�F@��@�@�@���@���@�?}@��@�  @�\)@�M�@�X@�r�@��;@ۮ@ڸR@ٲ-@��/@�l�@��@�b@�G�@���@�5?@�@ف@�X@�%@ؓu@�(�@��m@ׅ@��@�V@�X@� �@Ӯ@�o@��#@�bN@ύP@�|�@�;d@��@�Z@�  @̼j@�{@�n�@Ο�@�-@��@���@�j@�dZ@��@�v�@�@��`@�1'@���@�;d@Ƈ+@��@�/@�K�@�{@��/@��F@���@���@��h@��h@���@���@���@��h@�/@�9X@��@���@��H@�l�@��F@���@�K�@�
=@��!@��@�`B@�bN@�b@��;@��
@�dZ@��R@�ff@�-@��@�hs@���@�r�@��@���@��P@�;d@�
=@��@���@���@��+@���@�p�@��@���@���@��D@�1@�ƨ@��P@�dZ@�"�@���@�-@�J@���@��@��j@�j@�Z@�Z@�I�@�  @��@�t�@�;d@�
=@��\@�$�@���@�7L@�V@��@���@�%@���@��@��/@��D@� �@� �@�Z@�1'@���@���@��P@�dZ@�;d@���@���@��\@�n�@�-@��-@�G�@�&�@���@��@�Q�@�ƨ@�o@��\@�V@�-@�{@�@���@�O�@��/@�r�@�(�@�1@��
@��P@��@���@�-@���@���@�`B@�V@�Ĝ@���@�r�@�A�@��@���@�o@��R@��\@�~�@�~�@�n�@�M�@�-@��@���@���@�?}@��`@�Ĝ@���@�Q�@��;@�dZ@�o@���@�$�@�@���@�`B@�%@���@�Z@�9X@�(�@��@�  @��
@���@�C�@��@�~�@�@�X@��@��@���@���@�;d@�
=@��@���@�ff@�@���@�O�@�V@���@��`@���@��j@���@��D@�z�@�bN@�I�@� �@��
@���@��@�1@��/@�G�@�7L@���@�C�@�
=@}�@r��@i��@b�@[t�@SdZ@O;d@H��@A��@:^5@2^5@-��@(  @"�H@V@�u@��@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	��B	��B	��B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B
uB
 �B
=qB
s�B
��B
��B
�TBB-Bn�B��B�FB��BƨBȴB��B��B��B��B��B��B�
B�B�BB�TB�ZB�fB�sB�yB�yB�yB�yB�yB�yB�B�B�B�B�B�yB�B��B;dBgmBiyBffBaHBn�Bt�B�B�uB��B�B�'B�-B�9B�^BŢB��B��B��B��B��B��B��BÖB�^B�-B�B��B��B��B�oB�DB�+B�B�7B�DB�7B�%B�Bv�BbNBN�BA�B33B�B�B��B�wB�Bk�B]/BL�B;dB �B  B
�B
�B
�mB
�ZB
�5B
ɺB
�B
��B
�PB
~�B
VB
\B	�5B	�qB	��B	}�B	]/B	L�B	L�B	M�B	\)B	YB	Q�B	A�B	33B	%�B	�B	{B	JB	  B�B�yB�NB�5B�;B�mB�B�B�B�B�B�B�TB�B��BĜB�jB�'B�B�B��B�B�'B��B��B�uB�oB�oB�bB�\B�\B�VB�JB�JB�=B�DB�7B�+B�B�B�B�%B�+B�B}�B{�Bu�Bq�Bn�BgmBaHB_;B_;B_;B]/B[#BYB]/BbNBaHB`BB^5B\)BXBW
BT�BS�BXB[#BZBT�BT�BN�BI�BI�BI�BI�BH�BJ�BH�BF�BD�BC�BB�BF�BJ�BK�BL�BL�BK�BH�BH�BM�BM�BN�BP�BR�BVBW
B[#B^5B_;B`BBffBiyBl�Bo�Bv�B� B�bB��B��B��B�B�B�B�-B�9B�FB�?B�?B�LB�^B�wBŢBƨBǮBɺB��B��B�
B�B�
B��B��B�
B�NB�B��B��B	B	B	+B	DB	PB	bB	uB	�B	�B	�B	�B	"�B	#�B	%�B	&�B	%�B	$�B	&�B	%�B	&�B	(�B	,B	/B	1'B	2-B	49B	8RB	<jB	>wB	C�B	G�B	M�B	P�B	Q�B	Q�B	S�B	ZB	\)B	\)B	\)B	^5B	`BB	bNB	iyB	iyB	jB	jB	l�B	m�B	o�B	q�B	q�B	r�B	u�B	w�B	{�B	}�B	~�B	~�B	� B	�B	�B	�B	�1B	�7B	�7B	�7B	�7B	�7B	�1B	�7B	�=B	�JB	�\B	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�3B	�9B	�FB	�RB	�^B	�dB	�jB	�wB	�}B	�}B	�}B	��B	ÖB	ĜB	ĜB	ĜB	ÖB	ĜB	ĜB	ŢB	ŢB	ŢB	ƨB	ŢB	ŢB	ƨB	ƨB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�)B	�5B	�5B	�;B	�BB	�BB	�HB	�NB	�`B	�fB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�ZB	�NB	�;B	�5B	�5B	�5B	�5B	�5B	�5B	�5B	�5B	�;B	�;B	�;B	�;B	�BB	�BB	�BB	�HB	�HB	�NB	�`B	�fB	�mB	�mB	�B	�B	��B	��B	��B	��B
VB
{B
�B
"�B
)�B
2-B
8RB
<jB
B�B
I�B
Q�B
ZB
]/B
`BB
gmB
k�B
p�B
p�B
v�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B	��B	��B	��B	�B	�B	�B	�B	�B	��B	��B	��B	��B
B
�B
&�B
D�B
x�B
��B
��B
�HB
��B(�Bl�B��B�FB��BƨBȴB��B��B��B��B��B��B�B�B�BB�TB�ZB�fB�sB�yB�yB�yB�yB�yB�yB�B�B�B�B�B�B��B�BK�Bt�Bv�B{�Bm�Br�Bz�B�PB��B�'B�LB�XB�dB��BƨB��B��B��B�
B�B�B��B��B��BB�XB�?B�?B��B��B��B�{B�PB�+B�PB�\B�JB�JB�DB�1Bp�BYBO�BF�B7LBB�B�)B�DBw�Bl�B^5BS�B33B	7B
��B
�B
�B
�B
��B
�NB
�RB
��B
��B
��B
|�B
-B	��B	�/B	ĜB	��B	n�B	T�B	W
B	VB	hsB	l�B	gmB	O�B	E�B	7LB	&�B	�B	�B	hB	B��B�B�ZB�ZB�B��B��B��B�B��B��B�B�fB�
B��BǮB�LB�B�'B�-B�LB�qB�dB�B��B��B��B��B��B��B�oB�oB��B��B�{B�\B�PB�PB�7B�=B�\B�hB�1B�B�B{�By�Bx�Bs�BiyBjBgmBcTBbNBcTBbNBcTBhsBhsBgmBffBe`B^5B^5BZBW
B]/BdZBiyBaHBaHBZBM�BM�BM�BM�BN�BT�BO�BL�BJ�BI�BH�BL�BN�BN�BP�BS�BT�BQ�BP�BW
BQ�BQ�BS�BVBYB[#B_;BaHBaHBbNBjBm�Bp�Bs�Bv�B{�B�PB��B��B�B�B�B�'B�9B�FB�LB�LB�RB�LB�wB�wBŢBƨBǮBɺB��B�B�B�/B�
B��B��B��B�NB�B��B��B	B	B	
=B	PB	\B	bB	�B	�B	�B	�B	�B	%�B	(�B	,B	,B	+B	(�B	&�B	%�B	&�B	(�B	,B	/B	1'B	2-B	49B	;dB	>wB	@�B	C�B	F�B	M�B	Q�B	Q�B	R�B	VB	]/B	\)B	_;B	]/B	_;B	`BB	dZB	k�B	iyB	k�B	k�B	n�B	m�B	q�B	r�B	r�B	s�B	v�B	x�B	|�B	}�B	� B	~�B	� B	�B	�B	�%B	�7B	�7B	�DB	�=B	�=B	�7B	�1B	�7B	�JB	�JB	�\B	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�9B	�9B	�LB	�RB	�^B	�jB	�qB	�}B	��B	��B	��B	B	ĜB	ŢB	ŢB	ŢB	ŢB	ƨB	ƨB	ŢB	ƨB	ƨB	ǮB	ƨB	ŢB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�/B	�;B	�;B	�BB	�HB	�HB	�NB	�TB	�fB	�mB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�mB	�`B	�BB	�;B	�;B	�;B	�;B	�;B	�;B	�;B	�;B	�;B	�;B	�;B	�;B	�BB	�BB	�BB	�HB	�HB	�TB	�fB	�mB	�mB	�fB	�yB	�B	��B	��B	��B	��B
VB
{B
�B
"�B
)�B
2-B
8RB
<jB
C�B
J�B
Q�B
ZB
]/B
`BB
hsB
k�B
p�B
p�B
v�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<e`B<��<�o<D��<T��<�1<D��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<T��<D��<#�
<#�
<#�
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
<�o<e`B<#�
<e`B<���<�`B<�9X<���<�h<#�
<49X<u<�C�<ě�<�t�<#�
<#�
<#�
<#�
<49X<ě�<ě�<49X<#�
<�C�<�=��<�h<�h<��=�P<�h<�C�<#�
<#�
<#�
<D��<���<�1<e`B<�t�<�C�<49X<#�
<D��<�C�<e`B<D��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<u<e`B<#�
<#�
<49X<#�
<#�
<#�
<#�
<#�
<D��<���<�o<#�
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
<#�
<#�
<#�
<#�
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
<u<D��<D��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.3 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201101451592012011014515920120110145159  AO  ARGQ                                                                        20111130143654  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130143654  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120110145159  IP                  G�O�G�O�G�O�                