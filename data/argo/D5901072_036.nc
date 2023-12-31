CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-28T23:01:49Z UW 3.1 conversion   
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
_FillValue                    � Argo profile    3.1 1.2 19500101000000  5901072 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               $A   AO  20111130143845  20190522121828  1728_5048_036                   2C  D   APEX                            2142                            040306                          846 @ԄK`��1   @ԄK�[@5����m�c<I�^5?1   GPS     Primary sampling: mixed [deeper than nominal 990dbar: discrete; nominal 990dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�33@�  @���AffA@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bw��B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C�fC
  C  C  C  C  C  C  C  C  C�C�C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH�CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Ce�fCh  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DBfDB�fDC  DC� DC��DDy�DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Dn��Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dy�3D���D�9�D�VfD�ɚD�  D�&fD�� D��3D�� D�  D�\�D��fD��3D� D�p Dਗ਼D�ٚD�&fD�c3D�p 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @���@�ff@�33A��A;33A[33A{33A���A���A���A���A͙�Aݙ�A홚A���B��B��B��B��B&��B.��B6��B>��BF��BN��BV��B^��Bf��Bn��BvffB~��B�ffB�ffB�ffB�33B�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB���B�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffC�3C�3C�3C��C	�3C�3C�3C�3C�3C�3C�3C�3C�3C��C��C�3C!�3C#�3C%�3C'�3C)�3C+�3C-�3C/�3C1�3C3�3C5�3C7�3C9�3C;�3C=�3C?�3CA�3CC�3CE�3CG��CI�3CK�3CM�3CO�3CQ�3CS�3CU�3CW�3CY�3C[�3C]�3C_�3Ca�3Cc�3Ce��Cg�3Ci�3Ck�3Cm�3Co�3Cq�3Cs�3Cu�3Cw�3Cy�3C{�3C}�3C�3C�ٚC�ٚC�ٚC�ٚC��fC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC��fC��fC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC��fC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚD l�D ��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��D	l�D	��D
l�D
��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��D l�D ��D!l�D!��D"l�D"��D#l�D#��D$l�D$��D%l�D%��D&l�D&��D'l�D'��D(l�D(��D)l�D)��D*l�D*��D+l�D+��D,l�D,��D-l�D-��D.l�D.��D/l�D/��D0l�D0��D1l�D1��D2l�D2��D3l�D3��D4l�D4��D5l�D5��D6l�D6��D7l�D7��D8l�D8��D9l�D9��D:l�D:��D;l�D;��D<l�D<��D=l�D=��D>l�D>��D?l�D?��D@l�D@��DAl�DA�3DBs3DB��DCl�DC�fDDffDD��DEl�DE��DFl�DF��DGl�DG��DHl�DH��DIl�DI��DJl�DJ��DKl�DK��DLl�DL��DMl�DM��DNl�DN��DOl�DO��DPl�DP��DQl�DQ��DRl�DR��DSl�DS��DTl�DT��DUl�DU��DVl�DV��DWl�DW��DXl�DX��DYl�DY��DZl�DZ��D[l�D[��D\l�D\��D]l�D]��D^l�D^��D_l�D_��D`l�D`��Dal�Da��Dbl�Db��Dcl�Dc��Ddl�Dd��Del�De��Dfl�Df��Dgl�Dg��Dhl�Dh��Dil�Di��Djl�Dj��Dkl�Dk��Dll�Dl��Dml�Dm��Dnl�Dn�fDol�Do��Dpl�Dp��Dql�Dq��Drl�Dr��Dsl�Ds��Dtl�Dt��Dul�Du��Dvl�Dv��Dwl�Dy� D�� D�0 D�L�D�� D��fD��D��fD���D��fD�fD�S3DǼ�D�ٚD�fD�ffD� D�� D��D�Y�D�ff11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�VA�{A�oA��A��A���A�\)A�1A��A���A�l�A��AǼjAǃA�XA�I�A�=qA�1'A�$�A�oA�1A�A���A���A��A��HA���AƍPAŗ�A�p�A�ĜA�A�A�A��mA���A���A��A�\)A�`BA���A���A�7LA���A��wA�O�A��A�G�A�dZA���A��/A�x�A�7LA�A�A��-A�p�A�{A���A�ffA�%A�A��!A��uA���A�$�A��A�\)A��A�bA��hA�  A�E�A�ZA��/A�oA�VA���A�bA��\A��A�ĜA��FA�E�A�\)A�1A�ĜA�{A��+A�r�A�A�A��
A�
=A�+A�(�A�ƨA���A���A�=qA���A�A�1A�x�A�XA�bA��A�?}A�n�A�VA���A��A��A��A�n�A~JA{�AyoAw�;AvZAr1'AqdZAoXAm|�Aj��Ah�jAf��Ad�Ab1'A_C�A\1AZ�DAX�\AWoAVI�AUƨAUO�AT~�AR��AQVAP9XAO�ANJALjAIoAG��AF��AE�TADbNAC`BAB�jAA��A?��A>�DA=�hA=/A<��A:ȴA9`BA7��A6��A65?A4�A3��A3A2�RA2n�A2$�A.��A-��A-�7A,�/A+��A*�A)��A(�RA'�A';dA&��A$�RA#+A"�HA"�RA!�A =qA�
A��AbAdZA�AK�An�A33A  A?}A �A��AbNA�#A�7A=qA��A��A1A;dAbNAl�A�RA��A
�HA
ZA	�
A�RA�FA`BA�mA&�Ar�A�A�AK�A�A��A�A{A7L@��
@��R@���@�j@��@��^@��@�n�@��@�(�@���@���@�I�@�~�@�O�@���@�b@�ff@��/@��@��@�-@�l�@�?}@߾w@��@ڧ�@�\)@�%@�K�@҇+@�@�p�@Ѓ@�+@�$�@�G�@̛�@˥�@�5?@�p�@�Q�@�l�@�n�@�@�&�@�Ĝ@�O�@���@�9X@�+@��@��@�t�@��+@�p�@�1@��F@�S�@���@�`B@�X@��@��@�j@�(�@���@�K�@�
=@�o@�;d@�"�@���@��@�O�@�O�@�%@�Z@��P@���@�l�@��
@��@���@��m@�(�@�  @�ƨ@���@�b@�1@��@�\)@��-@��#@�V@���@��@�5?@��@��!@�|�@���@�
=@��H@��H@�?}@�j@�Z@�r�@���@��!@��R@��H@��;@�V@��`@�r�@�ƨ@�bN@���@�%@�O�@�G�@��m@�$�@���@��@��h@�$�@��@��@�S�@���@��@�S�@���@���@�1'@���@��w@���@�S�@��R@���@�(�@�t�@�ȴ@�~�@�=q@���@�V@�Ĝ@��@�r�@��m@���@��@��@��H@��9@���@��
@��w@���@�S�@�C�@�"�@��@��@�=q@���@�r�@��@�z�@�bN@�1'@��@�|�@�
=@�ff@��@�o@�E�@��-@��@�S�@���@���@���@�9X@��F@�t�@�dZ@�;d@���@��R@���@�~�@�E�@�J@��#@���@�G�@��@��/@��j@���@���@��j@�I�@���@��;@���@�\)@��!@�E�@�J@���@��@��@��#@��^@�X@��`@��u@�1'@���@�ƨ@���@�;d@���@��!@��+@�ff@�{@��@�@��@�hs@�?}@��@��9@�j@�9X@���@��
@��w@�K�@��H@�^5@��@���@�hs@�O�@�X@�G�@��@�?}@�hs@��@�p�@�7L@��j@��m@���@���@�C�@�
=@���@�p�@y%@rn�@j�!@` �@U�h@O;d@HQ�@C33@=�@4��@0�u@+�
@$��@ Q�@�D@�7@�@M�@�@bN11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�VA�{A�oA��A��A���A�\)A�1A��A���A�l�A��AǼjAǃA�XA�I�A�=qA�1'A�$�A�oA�1A�A���A���A��A��HA���AƍPAŗ�A�p�A�ĜA�A�A�A��mA���A���A��A�\)A�`BA���A���A�7LA���A��wA�O�A��A�G�A�dZA���A��/A�x�A�7LA�A�A��-A�p�A�{A���A�ffA�%A�A��!A��uA���A�$�A��A�\)A��A�bA��hA�  A�E�A�ZA��/A�oA�VA���A�bA��\A��A�ĜA��FA�E�A�\)A�1A�ĜA�{A��+A�r�A�A�A��
A�
=A�+A�(�A�ƨA���A���A�=qA���A�A�1A�x�A�XA�bA��A�?}A�n�A�VA���A��A��A��A�n�A~JA{�AyoAw�;AvZAr1'AqdZAoXAm|�Aj��Ah�jAf��Ad�Ab1'A_C�A\1AZ�DAX�\AWoAVI�AUƨAUO�AT~�AR��AQVAP9XAO�ANJALjAIoAG��AF��AE�TADbNAC`BAB�jAA��A?��A>�DA=�hA=/A<��A:ȴA9`BA7��A6��A65?A4�A3��A3A2�RA2n�A2$�A.��A-��A-�7A,�/A+��A*�A)��A(�RA'�A';dA&��A$�RA#+A"�HA"�RA!�A =qA�
A��AbAdZA�AK�An�A33A  A?}A �A��AbNA�#A�7A=qA��A��A1A;dAbNAl�A�RA��A
�HA
ZA	�
A�RA�FA`BA�mA&�Ar�A�A�AK�A�A��A�A{A7L@��
@��R@���@�j@��@��^@��@�n�@��@�(�@���@���@�I�@�~�@�O�@���@�b@�ff@��/@��@��@�-@�l�@�?}@߾w@��@ڧ�@�\)@�%@�K�@҇+@�@�p�@Ѓ@�+@�$�@�G�@̛�@˥�@�5?@�p�@�Q�@�l�@�n�@�@�&�@�Ĝ@�O�@���@�9X@�+@��@��@�t�@��+@�p�@�1@��F@�S�@���@�`B@�X@��@��@�j@�(�@���@�K�@�
=@�o@�;d@�"�@���@��@�O�@�O�@�%@�Z@��P@���@�l�@��
@��@���@��m@�(�@�  @�ƨ@���@�b@�1@��@�\)@��-@��#@�V@���@��@�5?@��@��!@�|�@���@�
=@��H@��H@�?}@�j@�Z@�r�@���@��!@��R@��H@��;@�V@��`@�r�@�ƨ@�bN@���@�%@�O�@�G�@��m@�$�@���@��@��h@�$�@��@��@�S�@���@��@�S�@���@���@�1'@���@��w@���@�S�@��R@���@�(�@�t�@�ȴ@�~�@�=q@���@�V@�Ĝ@��@�r�@��m@���@��@��@��H@��9@���@��
@��w@���@�S�@�C�@�"�@��@��@�=q@���@�r�@��@�z�@�bN@�1'@��@�|�@�
=@�ff@��@�o@�E�@��-@��@�S�@���@���@���@�9X@��F@�t�@�dZ@�;d@���@��R@���@�~�@�E�@�J@��#@���@�G�@��@��/@��j@���@���@��j@�I�@���@��;@���@�\)@��!@�E�@�J@���@��@��@��#@��^@�X@��`@��u@�1'@���@�ƨ@���@�;d@���@��!@��+@�ff@�{@��@�@��@�hs@�?}@��@��9@�j@�9X@���@��
@��w@�K�@��H@�^5@��@���@�hs@�O�@�X@�G�@��@�?}@�hs@��@�p�@�7L@��j@��m@���@���@�C�@�
=@���@�p�@y%@rn�@j�!@` �@U�h@O;d@HQ�@C33@=�@4��@0�u@+�
@$��@ Q�@�D@�7@�@M�@�@bN11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�9B�9B�9B�9B�3B�!B�B��B�B�B�B�-B�RB�dB�jB�qB�wB�}B��B��BBBĜBĜBĜBĜBĜBŢB��B�wB��B�HB�mB�mB�/B�)B�5B�5B�`B�5B�B�B�#B�#B�#B�B�#B�BB�)B�5B�NB�fB�mB�B�B�B�B�B�B��B��B��B�B�B�ZB�B��BƨB��B�}B�dB�LB��B��B��B�JB�%B�By�Bo�Bm�BS�B?}B�B�B�B�B��B�TB�;B�B��B�dB��B�bB|�B`BBR�BG�B<jB/B#�B �B�BB
�sB
ŢB
��B
�PB
cTB
I�B
1'B
JB	�B	�
B	��B	�9B	��B	��B	�wB	�^B	�^B	��B	��B	�bB	�%B	v�B	_;B	O�B	I�B	B�B	49B	0!B	'�B	%�B	!�B	 �B	uB	bB	bB	+B��B�B�mB�TB�5B�B��B��B��BǮB�jB�FB�9B�!B�B��B�B��B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�uB�hB�{B�hB�VB�PB�VB�JB�+B�B�B~�B{�Bw�Br�Bo�Bo�Bo�Bs�Bt�B|�B�B�B|�B~�B}�By�B|�B{�Bz�Bu�Bx�B|�B�Bx�B{�Bs�Bu�Bw�Bw�Bx�Bz�Bx�Bv�Bw�Bt�Bt�Br�Br�Bn�Bo�Bo�Bq�Bm�Bm�Bv�Bq�Bk�BgmBffBhsBiyBhsBhsBiyBiyBgmBcTBcTBcTBbNBcTBaHBbNBcTBbNBbNBaHBbNBcTB`BB`BB`BB`BB]/B_;BcTBffBe`Bm�Bt�Bx�B�B�=B�\B�hB�PB�VB�=B�B�B�B�B�bB�\B�B�PB�{B��B��B��B��B��B��B��B�B�B�!B�FB�3B�?B�FB�XB�FBÖBȴB��B�B�B��B��B��B��B	  B	B	B	B	1B	B	JB	JB	DB	DB	bB	+B	B	PB	{B	�B	%�B	-B	)�B	+B	49B	9XB	:^B	;dB	6FB	<jB	F�B	S�B	_;B	\)B	[#B	cTB	gmB	gmB	k�B	p�B	u�B	k�B	aHB	ffB	n�B	n�B	r�B	v�B	w�B	}�B	�B	�=B	�7B	�7B	|�B	y�B	m�B	o�B	q�B	n�B	iyB	e`B	ffB	hsB	gmB	hsB	iyB	iyB	jB	n�B	o�B	n�B	o�B	r�B	� B	�PB	��B	�B	�B	�B	�B	�!B	�!B	�!B	�'B	�!B	�'B	�-B	�-B	�3B	�9B	�FB	�RB	�XB	�XB	�RB	�FB	�3B	�'B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�!B	�-B	�-B	�LB	�RB	�^B	�jB	�wB	�}B	B	ŢB	ƨB	ǮB	ƨB	ƨB	ǮB	ǮB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�#B	�)B	�)B	�/B	�5B	�5B	�;B	�BB	�BB	�HB	�NB	�TB	�ZB	�`B	�`B	�fB	�mB	�fB	�`B	�ZB	�ZB	�ZB	�ZB	�`B	�fB	�fB	�fB	�yB	�B	�B	�B	�B	�B	�B	�B	�yB	�B	�B	�B	��B
B
JB
hB
 �B
.B
5?B
;dB
>wB
E�B
L�B
R�B
XB
]/B
cTB
gmB
iyB
m�B
r�B
y�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�9B�9B�9B�9B�9B�-B�B�B�B�B�!B�9B�XB�jB�jB�qB�wB�}B��B��BBBĜBĜBĜBĜBŢBǮBƨB��B�#B�`B�yB�B�TB�HB�NB�ZB�B�ZB�)B�)B�/B�5B�5B�5B�BB�ZB�;B�BB�TB�mB�sB�B�B�B�B�B�B��B��B��B��B��B�B�5B�BɺBÖBÖB��B�qB�B�B��B�\B�=B�1B|�Bv�Br�B[#BH�B�B�B�B�B  B�ZB�HB�)B��B��B�B��B�BdZBVBJ�BA�B2-B$�B"�B"�B\B
�B
��B
�'B
��B
jB
O�B
;dB
�B	��B	�;B	ĜB	�XB	�'B	��B	ÖB	�wB	��B	��B	��B	��B	�=B	}�B	gmB	S�B	N�B	F�B	6FB	2-B	)�B	(�B	&�B	&�B	�B	{B	{B	PB	+B��B�B�fB�TB�#B��B�B��B��B�}B�RB�FB�FB�!B�B�!B�B�'B�'B��B��B��B�B��B��B��B��B��B��B��B�B��B��B��B��B��B��B�{B��B��B�uB�oB�\B�bB�bB�=B�+B�B�B~�B{�Bv�Bq�Bq�Bq�Bw�Bv�B�B�B�B� B�B�B|�B�B}�B|�Bx�Bz�B}�B�Bz�B}�Bt�Bv�Bx�Bx�By�B{�By�By�B{�Bv�Bv�Bt�Bt�Bp�Bq�Bs�Bs�Bo�Bo�Bx�Bt�Bn�BiyBgmBjBl�Bk�BjBk�Bk�Bk�BgmBffBhsBffBiyBe`Be`Be`BcTBcTBcTBdZBe`BbNBbNBbNBcTB_;BaHBe`BhsBffBo�Bu�Bw�B�%B�JB�oB��B�bB�\B�JB�+B�B�%B�%B�oB�bB�B�VB��B��B��B��B��B��B��B��B�B�B�-B�LB�3B�FB�LB�^B�FBĜBȴB��B�B�B��B��B	  B��B	  B	B	B	%B	DB	B	VB	PB	PB	VB	uB	
=B	B	PB	�B	�B	%�B	0!B	,B	+B	49B	:^B	;dB	;dB	6FB	;dB	E�B	S�B	`BB	]/B	ZB	cTB	gmB	gmB	k�B	r�B	x�B	m�B	`BB	e`B	m�B	m�B	r�B	v�B	w�B	}�B	�B	�DB	�DB	�JB	�B	{�B	m�B	p�B	r�B	p�B	l�B	ffB	gmB	iyB	hsB	iyB	jB	jB	jB	o�B	p�B	o�B	o�B	p�B	|�B	�=B	��B	�B	�B	�B	�!B	�!B	�!B	�!B	�'B	�'B	�3B	�3B	�-B	�3B	�9B	�LB	�XB	�^B	�^B	�^B	�XB	�LB	�3B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�'B	�-B	�-B	�LB	�RB	�dB	�qB	�wB	�}B	ÖB	ƨB	ǮB	ȴB	ƨB	ƨB	ǮB	ǮB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�#B	�)B	�/B	�/B	�;B	�;B	�;B	�HB	�HB	�NB	�TB	�ZB	�`B	�`B	�`B	�mB	�sB	�mB	�fB	�`B	�ZB	�ZB	�ZB	�`B	�fB	�fB	�fB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B
B
JB
hB
 �B
/B
5?B
;dB
>wB
E�B
L�B
R�B
YB
^5B
cTB
gmB
iyB
m�B
r�B
z�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<49X<#�
<#�
<49X<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.3 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201101452052012011014520520120110145205  AO  ARGQ                                                                        20111130143845  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130143845  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120110145205  IP                  G�O�G�O�G�O�                