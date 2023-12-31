CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-28T23:01:48Z UW 3.1 conversion   
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
_FillValue                    � Argo profile    3.1 1.2 19500101000000  5901072 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               !A   AO  20111130143825  20190522121828  1728_5048_033                   2C  D   APEX                            2142                            040306                          846 @�|�ߒ�1   @�|�l�@5��-V�cV��"��1   GPS     Primary sampling: mixed [deeper than nominal 990dbar: discrete; nominal 990dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @333@�  @�  A��A!��A@  A`  A�  A�  A�  A�  A�33A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@ffBH  BO��BX  B`  BhffBp  Bw��B�  B�  B�  B�  B���B���B�  B�  B�  B�33B�33B�33B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B���B���B�  B�33B�33B�33C �C  C�fC  C  C
�C�C  C�fC  C  C  C�C  C  C�C   C!�fC$  C&  C(  C*  C,�C.  C0  C2�C4  C6  C8  C:  C<�C>  C@  CB�CD  CE�fCH  CJ  CL�CN�CO�fCQ�fCS�fCU�fCX  CY�fC[�fC^  C`�Cb  Cd  Cf  Cg�fCi�fCl  Cn�Cp  Cr  Cs�fCv  Cx�Cz  C|  C~�C�  C�  C��C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C��C��3C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C��3C�  C�  C�  C��C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C��C�  C��3C�  C�  C�  C�  C��C�  C�  C�  C��3C��3C��3C��3C�  C�  C��C�  C�  C��C��C�  C�  C��3C��3C�  C��C�  C�  C��3C��3C�  C�  C�  C��C�  C�  C�  C�  C��C�  C�  C��C�  C��3C�  C��C�  C�  C��3C�  C��C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C��C�  C�  C��C��3C�  C�  C�  C��C�  C��3C�  C�  C�  C��C�  C�  C��C�  C�  C�  D   D � D  D� D  D� D  D�fD  Dy�D��D� D  D� D  D� DfD� D��D	y�D
  D
� D  D�fDfD�fDfD�fD  Dy�D  D�fDfD�fDfD�fDfD�fD  D� D  Dy�D  D� D��D� D  D� D  D� D  D� DfD�fD  D� D��Dy�D��D� DfD�fDfD� D   D � D!  D!� D"  D"� D#  D#y�D$  D$� D%  D%� D%��D&� D'  D'� D(  D(� D)  D)y�D*  D*� D+  D+� D,fD,� D-  D-� D.  D.y�D/  D/� D0  D0� D1  D1� D2  D2� D2��D3� D4  D4� D5fD5� D5��D6� D7fD7� D8  D8� D8��D9� D:  D:� D;  D;y�D<  D<� D=  D=� D=��D>� D?fD?� D@  D@�fDAfDA�fDBfDB�fDCfDC� DC��DDy�DE  DE�fDF  DF� DGfDG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL�fDM  DMy�DN  DN� DO  DO� DP  DP� DQ  DQ� DQ��DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[fD[� D\  D\� D]  D]� D^  D^�fD_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De�fDf  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� DqfDq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dy�3D��fD�fD���D��3D��fD�0 D�� D��3D�� D�,�D�Y�Dǳ3D��fD�,�D�vfD��D��fD�3D�Vf11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @   @l��@�ff@���A��A;33A[33A{33A���A���A���A���A͙�Aݙ�A홚A���B��B��B��B��B&��B.��B6��B?33BF��BNffBV��B^��Bg33Bn��BvffB~��B�ffB�ffB�ffB�33B�33B�ffB�ffB�ffB���B���B���B�ffB�ffB���B�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�33B�33B�33B�ffB�B���B���B���C�3C��C�3C�3C	��C��C�3C��C�3C�3C�3C��C�3C�3C��C�3C!��C#�3C%�3C'�3C)�3C+��C-�3C/�3C1��C3�3C5�3C7�3C9�3C;��C=�3C?�3CA��CC�3CE��CG�3CI�3CK��CM��CO��CQ��CS��CU��CW�3CY��C[��C]�3C_��Ca�3Cc�3Ce�3Cg��Ci��Ck�3Cm��Co�3Cq�3Cs��Cu�3Cw��Cy�3C{�3C}��C�3C�ٚC��fC�ٚC�ٚC�ٚC�ٚC�ٚC���C�ٚC�ٚC�ٚC�ٚC��fC���C���C���C�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC��fC��fC�ٚC���C�ٚC�ٚC�ٚC��fC�ٚC�ٚC�ٚC�ٚC�ٚC���C�ٚC�ٚC�ٚC�ٚC��fC�ٚC���C�ٚC�ٚC�ٚC�ٚC��fC�ٚC�ٚC�ٚC���C���C���C���C�ٚC�ٚC��fC�ٚC�ٚC��fC��fC�ٚC�ٚC���C���C�ٚC��fC�ٚC�ٚC���C���C�ٚC�ٚC�ٚC��fC�ٚC�ٚC�ٚC�ٚC��fC�ٚC�ٚC��fC�ٚC���C�ٚC��fC�ٚC�ٚC���C�ٚC��fC�ٚC�ٚC�ٚC�ٚC�ٚC���C�ٚC�ٚC�ٚC�ٚC�ٚC��fC�ٚC�ٚC��fC���C�ٚC�ٚC�ٚC��fC�ٚC���C�ٚC�ٚC�ٚC��fC�ٚC�ٚC��fC�ٚC�ٚC�ٚC�ٚD l�D ��Dl�D��Dl�D��Ds3D��DffD�fDl�D��Dl�D��Dl�D�3Dl�D�fD	ffD	��D
l�D
��Ds3D�3Ds3D�3Ds3D��DffD��Ds3D�3Ds3D�3Ds3D�3Ds3D��Dl�D��DffD��Dl�D�fDl�D��Dl�D��Dl�D��Dl�D�3Ds3D��Dl�D�fDffD�fDl�D�3Ds3D�3Dl�D��D l�D ��D!l�D!��D"l�D"��D#ffD#��D$l�D$��D%l�D%�fD&l�D&��D'l�D'��D(l�D(��D)ffD)��D*l�D*��D+l�D+�3D,l�D,��D-l�D-��D.ffD.��D/l�D/��D0l�D0��D1l�D1��D2l�D2�fD3l�D3��D4l�D4�3D5l�D5�fD6l�D6�3D7l�D7��D8l�D8�fD9l�D9��D:l�D:��D;ffD;��D<l�D<��D=l�D=�fD>l�D>�3D?l�D?��D@s3D@�3DAs3DA�3DBs3DB�3DCl�DC�fDDffDD��DEs3DE��DFl�DF�3DGl�DG��DHl�DH��DIl�DI��DJl�DJ��DKl�DK��DLs3DL��DMffDM��DNl�DN��DOl�DO��DPl�DP��DQl�DQ�fDRl�DR��DSl�DS��DTl�DT��DUl�DU��DVl�DV��DWl�DW��DXl�DX��DYl�DY��DZl�DZ�3D[l�D[��D\l�D\��D]l�D]��D^s3D^��D_l�D_��D`l�D`��Dal�Da��Dbl�Db��Dcl�Dc��Ddl�Dd��Des3De��Dfl�Df��Dgl�Dg��Dhl�Dh��Dil�Di��Djl�Dj��Dkl�Dk��Dll�Dl��Dml�Dm��Dnl�Dn��Dol�Do��Dpl�Dp�3Dql�Dq��Drl�Dr��Dsl�Ds��Dtl�Dt��Dul�Du��Dvl�Dv��Dwl�Dy� D���D��D��3D���D���D�&fD�vfD���D��fD�#3D�P Dǩ�D���D�#3D�l�D�3D���D�	�D�L�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�l�A�l�A�hsA�p�A�t�A�v�A�x�A�v�A�n�A�t�A�v�A�ffA�ffA�^5A�=qAǼjA�bNA��A��A���A�ĜA�Aĺ^Aĩ�Aě�Aď\A�jA�+A�A���AÕ�A�|�A�t�A�M�A�{A���A���A�A�A��FA��A�K�A�
=A��A���A���A�;dA�VA�K�A���A�~�A�ȴA�n�A��jA�t�A��HA��#A��7A�v�A���A�G�A��A�bA�JA�A�v�A��TA�VA���A�  A��
A��DA���A�hsA��A��HA���A�C�A��A���A�E�A��9A��`A��PA���A���A�=qA��`A�z�A���A�1A��A�;dA��^A��A�bNA�p�A��A��7A�A�ZA���A�JA�=qA�A���A�v�A�x�A�A�A��!A�;dA��FA���A�dZA��`A�;dA��!A�~�A~��A}AvI�AsdZAsO�Ar�Aq"�Am��Ah��AeAc��A_�hA[�FAY;dAV��AU��ATjARz�AP~�AN��AN1AM�AMhsAMC�AL�HAK�TAG��AE��AD�yADJACl�AC+AB��ABbA?�A<5?A:I�A9�FA97LA7�A5�
A4�+A3�-A2�A/��A.{A-x�A-VA,$�A+�A*�A*E�A)ƨA'��A%��A$�HA#�A!dZAx�A�mAr�A`BA��AbA��AȴA�
AS�A��AM�A��A�`A-AXA{AI�Al�A�RA  AXA�`Ar�A��A;dA
ZA	��A	l�A	O�A	
=A��AjA-A�AA/An�A�A��AjAAC�AVA �A j@�"�@�7L@�I�@�+@�x�@���@�M�@�u@�h@�K�@��@��
@��@��@�x�@��y@��@�7L@�M�@��@�E�@��@��@��@��@�G�@�5?@���@�l�@͙�@�b@�V@�Q�@�l�@��H@�@��@�;d@�ff@��@�?}@�ƨ@�"�@��+@��T@�O�@�  @���@�hs@���@�Q�@�b@��@�K�@��@��@�ȴ@���@��+@���@�p�@�hs@���@�7L@���@�I�@��
@��@��;@�j@��/@�X@���@�/@�I�@��P@���@�$�@��/@���@�V@�V@��+@��@� �@�Z@�9X@�Z@�A�@�Z@�Z@���@�7L@�?}@�%@�?}@��@���@��7@�x�@���@��#@��@���@�&�@��j@�I�@�+@���@�X@��@���@�Ĝ@�I�@�\)@�-@��^@�`B@�%@��9@��@�bN@�A�@�(�@��@�1@�1@� �@�9X@� �@�b@��m@�
=@�X@��@�Z@�1@���@�o@��T@��@���@�v�@��-@��-@���@�G�@��`@���@�A�@��@�b@�(�@�ƨ@�@�ȴ@�o@�"�@�33@�S�@��P@�  @�  @���@�K�@��H@�v�@�$�@���@�hs@��@�Z@�|�@��@��R@�n�@�M�@�x�@��9@��@��u@��9@�V@�p�@���@���@���@��h@�`B@��9@���@��P@��P@��@��@�  @��m@��P@��w@� �@�A�@�A�@�I�@�j@��D@��u@��u@�z�@�Z@� �@��w@�\)@�C�@�K�@�33@�o@��y@���@�^5@�E�@�5?@�=q@���@��h@��@�V@��@�%@��D@��
@�o@���@� �@�A�@�9X@�b@���@�S�@�;d@�+@��@���@�J@�x�@���@�1'@���@�|�@���@���@�v�@�M�@�E�@�@��7@�&�@�Ĝ@�r�@�A�@��@�@K�@~V@}�h@}p�@}`B@|��@|z�@{�
@z�@z��@y��@y&�@y&�@z-@pbN@h��@`  @W��@Q��@G�@@bN@9�@3�@,�@&�@ ��@9X@l�@�\@\)@
�\@�@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�l�A�l�A�hsA�p�A�t�A�v�A�x�A�v�A�n�A�t�A�v�A�ffA�ffA�^5A�=qAǼjA�bNA��A��A���A�ĜA�Aĺ^Aĩ�Aě�Aď\A�jA�+A�A���AÕ�A�|�A�t�A�M�A�{A���A���A�A�A��FA��A�K�A�
=A��A���A���A�;dA�VA�K�A���A�~�A�ȴA�n�A��jA�t�A��HA��#A��7A�v�A���A�G�A��A�bA�JA�A�v�A��TA�VA���A�  A��
A��DA���A�hsA��A��HA���A�C�A��A���A�E�A��9A��`A��PA���A���A�=qA��`A�z�A���A�1A��A�;dA��^A��A�bNA�p�A��A��7A�A�ZA���A�JA�=qA�A���A�v�A�x�A�A�A��!A�;dA��FA���A�dZA��`A�;dA��!A�~�A~��A}AvI�AsdZAsO�Ar�Aq"�Am��Ah��AeAc��A_�hA[�FAY;dAV��AU��ATjARz�AP~�AN��AN1AM�AMhsAMC�AL�HAK�TAG��AE��AD�yADJACl�AC+AB��ABbA?�A<5?A:I�A9�FA97LA7�A5�
A4�+A3�-A2�A/��A.{A-x�A-VA,$�A+�A*�A*E�A)ƨA'��A%��A$�HA#�A!dZAx�A�mAr�A`BA��AbA��AȴA�
AS�A��AM�A��A�`A-AXA{AI�Al�A�RA  AXA�`Ar�A��A;dA
ZA	��A	l�A	O�A	
=A��AjA-A�AA/An�A�A��AjAAC�AVA �A j@�"�@�7L@�I�@�+@�x�@���@�M�@�u@�h@�K�@��@��
@��@��@�x�@��y@��@�7L@�M�@��@�E�@��@��@��@��@�G�@�5?@���@�l�@͙�@�b@�V@�Q�@�l�@��H@�@��@�;d@�ff@��@�?}@�ƨ@�"�@��+@��T@�O�@�  @���@�hs@���@�Q�@�b@��@�K�@��@��@�ȴ@���@��+@���@�p�@�hs@���@�7L@���@�I�@��
@��@��;@�j@��/@�X@���@�/@�I�@��P@���@�$�@��/@���@�V@�V@��+@��@� �@�Z@�9X@�Z@�A�@�Z@�Z@���@�7L@�?}@�%@�?}@��@���@��7@�x�@���@��#@��@���@�&�@��j@�I�@�+@���@�X@��@���@�Ĝ@�I�@�\)@�-@��^@�`B@�%@��9@��@�bN@�A�@�(�@��@�1@�1@� �@�9X@� �@�b@��m@�
=@�X@��@�Z@�1@���@�o@��T@��@���@�v�@��-@��-@���@�G�@��`@���@�A�@��@�b@�(�@�ƨ@�@�ȴ@�o@�"�@�33@�S�@��P@�  @�  @���@�K�@��H@�v�@�$�@���@�hs@��@�Z@�|�@��@��R@�n�@�M�@�x�@��9@��@��u@��9@�V@�p�@���@���@���@��h@�`B@��9@���@��P@��P@��@��@�  @��m@��P@��w@� �@�A�@�A�@�I�@�j@��D@��u@��u@�z�@�Z@� �@��w@�\)@�C�@�K�@�33@�o@��y@���@�^5@�E�@�5?@�=q@���@��h@��@�V@��@�%@��D@��
@�o@���@� �@�A�@�9X@�b@���@�S�@�;d@�+@��@���@�J@�x�@���@�1'@���@�|�@���@���@�v�@�M�@�E�@�@��7@�&�@�Ĝ@�r�@�A�@��@�@K�@~V@}�h@}p�@}`B@|��@|z�@{�
@z�@z��@y��@y&�@y&�@z-@pbN@h��@`  @W��@Q��@G�@@bN@9�@3�@,�@&�@ ��@9X@l�@�\@\)@
�\@�@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
ƨB
��B
�BB
�yB
�B
�B
�B
�B
�B
��B
��B
��BBbB�B�B$�B(�B.B1'B6FB5?B5?B7LBC�BI�BG�B`BB�B��BŢB��B��BJBJB�B.B,B)�B&�B"�B$�BG�BL�BO�BS�BT�B\)BdZBhsBjBiyBdZBP�BM�BP�BR�BR�BS�BS�BW
BQ�BJ�BE�B5?BoBhB'�B!�B�B�BhB	7B��B��B��B�ZB��B�dB�B7LBhB
��B
�`B
��B
�9B
��B
�=B
n�B
O�B
9XB
33B
1'B
D�B
H�B
D�B
:^B
0!B
(�B
!�B
\B	��B	ÖB	�!B	�-B	�'B	��B	�DB	s�B	p�B	r�B	bNB	E�B	6FB	0!B	.B	'�B	�B	�B	JB	+B	B	B	  B��B��B�NB��B��B��B��B��BɺBƨBŢB�jB�3B�B�B��B��B��B��B��B��B��B��B��B��B��B�{B��B��B��B�bB�DB�DB�DB�bB�oB�VB�JB�JB�JB�DB�PB�=B�+B�B�B�B�B�B�%B�%B~�By�Bu�Bs�Bo�Bm�Bk�BhsBgmBiyBjBk�BjBjBiyBiyBl�Bq�Bs�Br�Bp�BjBgmBdZBdZBcTBcTBjBn�Bq�Br�Bs�Bs�Bp�BgmB[#BP�BE�BA�B>wB@�B<jB9XB8RB33B2-B49B;dBVBM�B>wB;dBB�BG�BC�B=qB;dB8RB49B33B0!B-B-B,B.B1'B2-B5?B6FB8RB?}BB�BC�BD�BC�BG�BN�BXB[#B\)B^5BbNBffBm�Bt�Bx�Bx�Bx�By�B{�B�B�7B�\B�oB�{B��B��B�B�XBƨB��B�B�
B�
B�
B�B�B�B�
B��B��B��B��B��B�
B�;B�ZB�sB�B��B	B	B	1B		7B	JB	bB	�B	�B	�B	�B	�B	!�B	!�B	!�B	 �B	�B	�B	 �B	 �B	�B	�B	�B	�B	 �B	&�B	(�B	)�B	,B	-B	/B	0!B	1'B	2-B	33B	49B	6FB	9XB	>wB	E�B	F�B	E�B	D�B	F�B	F�B	F�B	F�B	F�B	D�B	F�B	E�B	F�B	E�B	E�B	I�B	Q�B	YB	]/B	^5B	_;B	`BB	e`B	ffB	hsB	iyB	l�B	q�B	s�B	u�B	v�B	z�B	�B	�B	�%B	�7B	�DB	�JB	�PB	�VB	�VB	�\B	�oB	�{B	��B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�-B	�-B	�-B	�-B	�'B	�-B	�-B	�3B	�?B	�LB	�dB	�wB	�wB	��B	B	ĜB	ŢB	ƨB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�#B	�)B	�5B	�5B	�;B	�BB	�BB	�HB	�5B	�NB	�fB	�sB	�yB	�yB	�yB	�yB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B	��B	��B	��B
JB
�B
 �B
(�B
/B
33B
:^B
?}B
D�B
H�B
O�B
XB
_;B
cTB
hsB
m�B
q�B
w�B
z�B
}�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�BB
ȴB
��B
�BB
�yB
�B
�B
�B
�B
�B
��B
��B
��BBhB�B�B&�B+B1'B6FB;dB7LB8RB;dBJ�BS�BZBp�B�PB�'B��B�NBBoB�B$�B6FB8RB8RB2-B+B'�BI�BM�BP�BW
B_;Bm�Bm�Bo�Bs�Bw�Bq�BYBQ�BT�BW
BW
B\)BcTBdZB\)BS�BQ�BC�B�B�B-B'�B%�B#�B�B�B1BB+B��B�TB�B��BI�B�BbB
��B
�B
ŢB
�!B
��B
�+B
^5B
D�B
C�B
7LB
G�B
M�B
L�B
D�B
;dB
:^B
33B
#�B
�B	��B	�3B	�^B	B	�jB	�B	�1B	�B	�\B	y�B	XB	E�B	9XB	9XB	5?B	-B	!�B	hB	
=B	%B	%B	+B	
=B	PB�B�B�B��B��B��B��B�#B�BȴB�RB�FB�FB�9B�B��B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�oB�hB�hB�oB�{B�VB�JB�1B�+B�1B�7B�JB�hB�oB�%B� B{�Bx�Bs�Bq�Bp�Bn�Bm�Bm�Bl�Bm�Bl�Bl�Bl�Bk�Bn�Bu�By�Bz�B{�Bt�Bo�BiyBgmBe`BffBo�Bu�Bw�Bw�By�B{�B}�Bu�BdZB\)BM�BH�BG�BJ�BD�B?}BA�B7LB1'B0!B8RBbNBZBC�B:^BE�BW
BM�BC�BB�B?}B:^B:^B7LB1'B1'B1'B49B5?B5?B8RB9XB=qB?}BE�BC�BD�BH�BL�BR�B[#B\)B\)B`BBbNBffBn�Bu�By�Bx�Bx�By�B{�B�B�JB�\B�{B�{B��B��B�B�XBƨB��B�B�B�B�
B�)B�/B�B�)B�B�B�B��B��B�B�;B�ZB�sB�B��B	B	B		7B		7B	JB	bB	�B	�B	�B	�B	�B	#�B	#�B	#�B	 �B	#�B	#�B	"�B	!�B	�B	�B	 �B	"�B	$�B	(�B	+B	)�B	-B	.B	0!B	1'B	1'B	2-B	33B	49B	6FB	9XB	?}B	F�B	H�B	I�B	I�B	H�B	G�B	H�B	F�B	I�B	H�B	J�B	E�B	J�B	E�B	E�B	I�B	S�B	ZB	^5B	^5B	`BB	_;B	e`B	hsB	hsB	jB	k�B	q�B	s�B	u�B	u�B	z�B	�B	�+B	�%B	�7B	�DB	�JB	�PB	�VB	�bB	�\B	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�-B	�3B	�-B	�?B	�'B	�3B	�-B	�3B	�?B	�FB	�jB	�}B	�qB	��B	B	ĜB	ŢB	ƨB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	�
B	�#B	�)B	�)B	�5B	�5B	�BB	�NB	�NB	�TB	�/B	�HB	�fB	�sB	�B	�B	�B	�yB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
  B	��B	��B
JB
�B
 �B
(�B
/B
33B
:^B
?}B
D�B
H�B
O�B
XB
_;B
cTB
hsB
m�B
q�B
w�B
z�B
}�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<�t�<�o<49X<#�
<#�
<T��<#�
<#�
<#�
<T��<#�
<D��<e`B<49X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<�C�<#�
<#�
<#�
<e`B<T��<#�
<#�
<#�
<#�
<#�
<#�
<u<T��<#�
<#�
<D��<e`B<#�
<#�
<#�
<#�
<#�
<#�
<#�
<T��<#�
<#�
<�o<�o<�t�<���=\)<�t�<D��<�C�<�o<�1<�C�<�C�<��
<ě�<e`B<49X<�o<#�
<#�
<#�
<#�
<#�
<49X<�C�<�C�<��
=�P<u<#�
<#�
<�o<�/<��<��
<��
<�`B<�j<�C�<u<#�
<#�
<T��<T��<#�
<#�
<#�
<#�
<#�
<#�
<T��<�9X<e`B<#�
<#�
<#�
<#�
<#�
<49X<���<���<D��<#�
<#�
<49X<T��<#�
<#�
<49X<�C�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<e`B<e`B<#�
<D��<u<e`B<49X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<D��<e`B<#�
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
<D��<D��<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.3 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201101452042012011014520420120110145204  AO  ARGQ                                                                        20111130143825  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130143825  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120110145204  IP                  G�O�G�O�G�O�                