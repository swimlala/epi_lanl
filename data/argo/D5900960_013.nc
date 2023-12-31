CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-01T23:26:48Z UW 3.1 conversion   
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7T   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    88   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8<   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8D   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~            8H   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8P   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8X   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8`   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8d   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8l   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9l   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9p   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9t   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9x   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9|   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  A`   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C\   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  K@   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M<   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  U    TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  _    TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  f�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  h�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  p�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  x�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  z�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �h   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �$   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �(   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �8   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �<   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �@   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �DArgo profile    3.1 1.2 19500101000000  5900960 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               A   AO  20111130103302  20190523124443  1514_5041_013                   2C  D   APEX                            2041                            062805                          846 @��B��1   @��B��@6Η�O�;�c,��E�1   GPS     Primary sampling: mixed [deeper than nominal 1000dbar: discrete; nominal 1000dbar to surface: 2dbar-bin averaged]                                                                                                                                                  A   A   A   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @y��@�ff@�ffA33A;33A[33A{33A���A���A���A���A͙�Aݙ�A홚A���B��B��B��B��B&��B.��B6��B>��BF��BN��BV��B^��Bf��Bn��Bv��B~��B�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffC�3C�3C�3C�3C	�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C!�3C#�3C%�3C'�3C)�3C+�3C-�3C/�3C1�3C3�3C5�3C7�3C9�3C;�3C=�3C?�3CA�3CC�3CE�3CG�3CI�3CK�3CM�3CO�3CQ�3CS�3CU�3CW�3CY�3C[�3C]�3C_�3Ca�3Cc�3Ce�3Cg�3Ci�3Ck�3Cm�3Co�3Cq�3Cs�3Cu�3Cw�3Cy�3C{�3C}�3C�3C�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚD l�D ��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��D	l�D	��D
l�D
��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��Dl�D��D l�D ��D!l�D!��D"l�D"��D#l�D#��D$l�D$��D%l�D%��D&l�D&��D'l�D'��D(l�D(��D)l�D)��D*l�D*��D+l�D+��D,l�D,��D-l�D-��D.l�D.��D/l�D/��D0l�D0��D1l�D1��D2l�D2��D3l�D3��D4l�D4��D5l�D5��D6l�D6��D7l�D7��D8l�D8��D9l�D9��D:l�D:��D;l�D;��D<l�D<��D=l�D=��D>l�D>��D?l�D?��D@l�D@��DAl�DA��DBl�DB��DCl�DC��DDl�DD��DEl�DE��DFl�DF��DGl�DG��DHl�DH��DIl�DI��DJl�DJ��DKl�DK��DLl�DL��DMl�DM��DNl�DN��DOl�DO��DPl�DP��DQl�DQ��DRl�DR��DSl�DS��DTl�DT��DUl�DU��DVl�DV��DWl�DW��DXl�DX��DYl�DY��DZl�DZ��D[l�D[��D\l�D\��D]l�D]��D^l�D^��D_l�D_��D`l�D`��Dal�Da��Dbl�Db��Dcl�Dc��Ddl�Dd��Del�De��Dfl�Df��Dgl�Dg��Dhl�Dh��Dil�Di��Djl�Dj��Dkl�Dk��Dll�Dl��Dml�Dm��Dnl�Dn��Dol�Do��Dpl�Dp��Dql�Dq��Drl�Dr��Dsl�Ds��Dtl�Dt��Dul�Du��Dvl�Dv��Dwl�Dw��Dxl�Dx��Dyl�Dy��Dzl�Dz��D{l�D{��D|l�D|��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�%A�%A�%A���A���AζFAΗ�A΃A�v�A�l�A�ffA�\)A�Q�A�K�A�+A�ƨA�  A�G�A˲-A��A��A�ffA�^5A���A��HA��A���A��A�ĜA�Q�A�r�A��A��A���A�9XA��jA���A�JA�+A��A�A�l�A��A��^A�dZA��A��A�A�7LA��A���A��A�~�A�K�A��/A�{A��#A���A�\)A�E�A���A���A�"�A��PA�
=A�x�A�jA��FA�(�A��FA�^5A��;A�~�A�/A���A��uA�  A��!A�JA��jA�~�A��RA��yA�=qA�VA��
A���A���A�JA��A���A��jA��A��A�A�A���A�^5A�?}A��;A��A��A�t�A�E�A��A��RA�(�A�G�A�M�A��A�"�A��9A���A��7A�E�A�7LA~��A}�FA|~�Az��AxVAw|�Aup�Asl�Aq�Ap��Ao�An(�Aj��Ah�Ag��AfZAd��Ac��Ac33Ab�HA`��A^��A]|�A\ĜA[�#AZM�AWt�AU�TAU��AU�hAT�+AR5?AP��AO?}AM�ALI�AKt�AK
=AI�hAH�AG�AE�wAD{AC|�AB�AAt�A@ �A>��A=�A<Q�A;�FA;l�A;oA:�`A:�A:��A:~�A9�A7�wA6E�A5;dA4jA3��A2ĜA29XA1�PA0ĜA/ƨA/G�A.��A.�RA.r�A.Q�A.  A-dZA-S�A-
=A,�RA,$�A+|�A*9XA)/A(�+A'VA& �A$�`A#�mA#XA#+A"bNA �jA�A��A��A?}A�+A(�AoA�;A�A��A�AĜAAVA�AdZA9XA7LA~�A�
A�yA��A
�+A	A`BA��An�Ap�A�HA1'A?}A1A�A �@��@�G�@�1'@�K�@��@�7L@��@���@���@�l�@���@�b@�{@��@�hs@��H@�V@�-@�V@�l�@��@�(�@ާ�@��@��@�$�@���@�@�%@�  @ҸR@�&�@�t�@͡�@�X@�&�@��m@�O�@�K�@�n�@��#@�&�@Õ�@¸R@�$�@�x�@��9@���@�"�@�v�@��7@�Z@�K�@���@�n�@���@��R@��@�Ĝ@� �@�33@�M�@���@���@�1'@�|�@��@�5?@�p�@���@�t�@��@�^5@��@�?}@���@���@�-@���@��`@�Z@��w@�33@�ff@�E�@�$�@���@�Ĝ@��@�S�@�33@�"�@���@�33@�C�@��@��@��@�ff@�&�@�A�@��w@�A�@�S�@��!@�=q@��@�G�@�bN@��@�t�@�+@��@��\@���@��`@�9X@�|�@���@�ȴ@�^5@���@��h@�`B@�hs@��@���@��!@�-@��@�G�@���@�-@���@�o@��;@��@��!@�$�@��#@�hs@�1'@��F@��T@���@�C�@�p�@�G�@��!@�9X@�Ĝ@��h@��+@�K�@��P@��
@��D@�X@��h@�@�@��h@�7L@��u@��@�x�@�`B@�&�@���@��`@���@��u@�9X@��@�1@��@��w@��@�C�@�
=@��R@�V@�{@���@��@��@���@��7@�O�@�7L@�&�@�&�@��j@��u@�z�@�bN@�A�@� �@��@�  @��;@�ƨ@��F@��@�|�@�l�@�dZ@�\)@�S�@��@�n�@�=q@�5?@��@��@���@�`B@���@�r�@�z�@�z�@�z�@�r�@�bN@�Z@�Q�@�A�@�A�@�(�@���@��
@��
@��
@��
@���@���@�t�@�S�@�C�@�o@��@���@��R@���@�n�@�J@���@�?}@��/@��@��@���@��@�Q�@��m@�t�@�+@���@���@�ff@�J@���@��^@���@�?}1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�%A�%A�%A���A���AζFAΗ�A΃A�v�A�l�A�ffA�\)A�Q�A�K�A�+A�ƨA�  A�G�A˲-A��A��A�ffA�^5A���A��HA��A���A��A�ĜA�Q�A�r�A��A��A���A�9XA��jA���A�JA�+A��A�A�l�A��A��^A�dZA��A��A�A�7LA��A���A��A�~�A�K�A��/A�{A��#A���A�\)A�E�A���A���A�"�A��PA�
=A�x�A�jA��FA�(�A��FA�^5A��;A�~�A�/A���A��uA�  A��!A�JA��jA�~�A��RA��yA�=qA�VA��
A���A���A�JA��A���A��jA��A��A�A�A���A�^5A�?}A��;A��A��A�t�A�E�A��A��RA�(�A�G�A�M�A��A�"�A��9A���A��7A�E�A�7LA~��A}�FA|~�Az��AxVAw|�Aup�Asl�Aq�Ap��Ao�An(�Aj��Ah�Ag��AfZAd��Ac��Ac33Ab�HA`��A^��A]|�A\ĜA[�#AZM�AWt�AU�TAU��AU�hAT�+AR5?AP��AO?}AM�ALI�AKt�AK
=AI�hAH�AG�AE�wAD{AC|�AB�AAt�A@ �A>��A=�A<Q�A;�FA;l�A;oA:�`A:�A:��A:~�A9�A7�wA6E�A5;dA4jA3��A2ĜA29XA1�PA0ĜA/ƨA/G�A.��A.�RA.r�A.Q�A.  A-dZA-S�A-
=A,�RA,$�A+|�A*9XA)/A(�+A'VA& �A$�`A#�mA#XA#+A"bNA �jA�A��A��A?}A�+A(�AoA�;A�A��A�AĜAAVA�AdZA9XA7LA~�A�
A�yA��A
�+A	A`BA��An�Ap�A�HA1'A?}A1A�A �@��@�G�@�1'@�K�@��@�7L@��@���@���@�l�@���@�b@�{@��@�hs@��H@�V@�-@�V@�l�@��@�(�@ާ�@��@��@�$�@���@�@�%@�  @ҸR@�&�@�t�@͡�@�X@�&�@��m@�O�@�K�@�n�@��#@�&�@Õ�@¸R@�$�@�x�@��9@���@�"�@�v�@��7@�Z@�K�@���@�n�@���@��R@��@�Ĝ@� �@�33@�M�@���@���@�1'@�|�@��@�5?@�p�@���@�t�@��@�^5@��@�?}@���@���@�-@���@��`@�Z@��w@�33@�ff@�E�@�$�@���@�Ĝ@��@�S�@�33@�"�@���@�33@�C�@��@��@��@�ff@�&�@�A�@��w@�A�@�S�@��!@�=q@��@�G�@�bN@��@�t�@�+@��@��\@���@��`@�9X@�|�@���@�ȴ@�^5@���@��h@�`B@�hs@��@���@��!@�-@��@�G�@���@�-@���@�o@��;@��@��!@�$�@��#@�hs@�1'@��F@��T@���@�C�@�p�@�G�@��!@�9X@�Ĝ@��h@��+@�K�@��P@��
@��D@�X@��h@�@�@��h@�7L@��u@��@�x�@�`B@�&�@���@��`@���@��u@�9X@��@�1@��@��w@��@�C�@�
=@��R@�V@�{@���@��@��@���@��7@�O�@�7L@�&�@�&�@��j@��u@�z�@�bN@�A�@� �@��@�  @��;@�ƨ@��F@��@�|�@�l�@�dZ@�\)@�S�@��@�n�@�=q@�5?@��@��@���@�`B@���@�r�@�z�@�z�@�z�@�r�@�bN@�Z@�Q�@�A�@�A�@�(�@���@��
@��
@��
@��
@���@���@�t�@�S�@�C�@�o@��@���@��R@���@�n�@�J@���@�?}@��/@��@��@���@��@�Q�@��m@�t�@�+@���@���@�ff@�J@���@��^@���@�?}1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB/B/B/B.B,B,B,B+B+B+B)�B)�B+B+B,B;dBB�B<jB7LB�B��B�B�ZB�B��B�B�B�TB��BB��BB+BB��B��B��B  BBB  B��B��B��B��B��B��B��B��B�B�B�B�fB�ZB�HB�/B�)B�B�B�B��BƨB�LB��B��B��B�VB�B|�By�Bv�Bq�Bp�Bm�Bk�BgmBaHBS�BL�BH�BB�B7LB,B{B��B�B�)B��B��B�9B��B��B�uB�By�Bt�BjBS�BK�B'�B�BuB  B
�NB
�B
��B
�XB
��B
��B
�B
v�B
jB
@�B
49B
/B
$�B
$�B
1'B
(�B
hB
�B
oB
	7B
  B	��B	�B	�ZB	��B	��B	ĜB	�wB	�RB	�9B	�-B	�B	��B	��B	�{B	�uB	�7B	�B	r�B	m�B	k�B	hsB	bNB	XB	L�B	G�B	<jB	6FB	2-B	2-B	1'B	&�B	#�B	�B	\B	JB		7B	  B��B�B�mB�TB�;B�/B�/B�)B�)B�)B�B��B��B��BȴBǮBŢBB�}B�wB�jB�dB�^B�^B�dB�dB�dB�^B�dB�dB�dB�dB�}B�wB�jB�XB�RB�?B�!B��B��B��B��B��B�oB�PB�1B�B~�B}�B~�B{�Bx�Br�Bp�Bn�BjBiyBgmBe`BhsB^5B`BB^5B`BB]/BVBW
BM�BD�BB�BA�BA�B?}B;dB:^B8RB6FB49B33B1'B0!B0!B.B-B,B+B+B'�B%�B#�B"�B �B�B�B�B�B�B�B�B�B�B�B�B{BuB{B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B&�B)�B/B49B49B6FB6FB8RB9XB<jB=qB>wB?}BA�BE�BF�BJ�BM�BM�BR�BYB[#B\)B_;B`BBaHBbNBffBffBffBiyBn�Bp�Bx�B� B�B�%B�PB�oB��B��B��B�B��B��B��B�'B�RB�qB�qB�wBBƨBǮBɺB��B��B��B��B��B��B��B�B�B�B�B�/B�TB�fB�B��B��B��B��B��B	B	+B	DB	{B	�B	�B	�B	�B	�B	�B	�B	�B	(�B	33B	$�B	 �B	#�B	.B	;dB	?}B	E�B	L�B	S�B	S�B	XB	_;B	iyB	m�B	r�B	{�B	�B	�B	�B	�+B	�PB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�'B	�-B	�-B	�-B	�?B	�FB	�FB	�LB	�RB	�XB	�XB	�^B	�^B	�^B	�^B	�^B	�qB	�wB	�wB	�wB	�wB	��B	ÖB	ŢB	ƨB	ƨB	ȴB	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�
B	�
B	�B	�B	�B	�B	�B	�)B	�/B	�5B	�5B	�;B	�;B	�;B	�;B	�;B	�HB	�HB	�NB	�TB	�ZB	�`B	�mB	�mB	�m1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B/B/B/B/B,B-B,B+B+B+B+B+B+B+B-B=qBE�B>wB@�B!�BB��B�B�5B�B�#B�/B�sB��BB  B%B1BB  B��BBBBBB��B��B��B��B��B��B��B��B�B�B�B�mB�fB�ZB�5B�/B�B�B�B��B��B�qB��B��B��B�bB�B~�Bz�Bx�Bs�Bq�Bo�Bl�BiyBgmBW
BN�BI�BF�B;dB49B�BB�B�BB��BĜB�XB�B��B��B�1B{�Bw�Bp�BVBQ�B(�B�B�BB
�ZB
�)B
��B
�jB
�B
��B
�B
{�B
s�B
E�B
9XB
2-B
&�B
'�B
5?B
/B
uB
�B
�B
VB
B	��B	��B	�B	��B	ĜB	ȴB	��B	�dB	�?B	�3B	�3B	��B	��B	��B	��B	�PB	�+B	u�B	m�B	l�B	jB	ffB	[#B	O�B	K�B	?}B	8RB	33B	5?B	33B	+B	&�B	 �B	bB	PB	PB	B��B�B�B�ZB�BB�;B�/B�/B�/B�/B�/B�B��B��B��BɺBȴBĜB��B��B�}B�jB�jB�dB�jB�jB�jB�jB�dB�jB�jB�qB��BB�}B�dB�jB�LB�9B�B��B��B��B��B��B�\B�DB�7B�B~�B�B~�Bz�Bt�Bq�Bo�Bl�Bk�BhsBhsBk�BaHBbNB`BBcTB`BBYB\)BQ�BE�BD�BD�BC�BA�B?}B>wB;dB9XB6FB6FB33B2-B2-B/B/B.B-B/B)�B(�B&�B$�B%�B!�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B!�B�B�B�B�B�B�B�B�B�B�B�B�B �B�B �B �B!�B$�B(�B+B0!B6FB6FB7LB8RB9XB;dB=qB>wB@�BA�BD�BF�BG�BK�BN�BP�BT�BZB\)B^5B`BBbNBbNBdZBffBffBgmBk�Bo�Br�By�B�B�B�%B�PB�oB��B��B��B�B��B��B��B�-B�XB�wB�wB�}BĜBƨBȴB��B��B��B��B��B��B��B��B�B�B�B�#B�/B�TB�`B�B��B��B��B��B��B	B	1B		7B	uB	�B	�B	�B	�B	�B	�B	�B	�B	&�B	:^B	'�B	 �B	!�B	,B	:^B	>wB	D�B	K�B	R�B	S�B	XB	^5B	iyB	m�B	r�B	|�B	�B	�B	�B	�%B	�PB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�-B	�-B	�-B	�3B	�?B	�FB	�LB	�RB	�RB	�XB	�XB	�^B	�dB	�dB	�dB	�dB	�qB	�wB	�wB	�}B	�}B	��B	ĜB	ƨB	ƨB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�
B	�
B	�B	�B	�B	�B	�B	�#B	�/B	�5B	�5B	�;B	�;B	�BB	�BB	�BB	�BB	�HB	�NB	�NB	�ZB	�`B	�fB	�mB	�sB	�m1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<T��<T��<49X<T��<D��<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<49X<#�
<#�
<#�
<#�
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
<D��<49X<49X<49X<#�
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
<49X<#�
<#�
<D��<49X<#�
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
<T��<49X<#�
<#�
<#�
<#�
<#�
<#�
<49X<49X<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.3 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201112181657242011121816572420111218165724  AO  ARGQ                                                                        20111130103302  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130103302  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20111218165724  IP                  G�O�G�O�G�O�                