CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:48Z UW 3.1 conversion   
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
_FillValue        G�O�   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  U|   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]p   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  _p   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    gd   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  id   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  qX   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yL   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  {L   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �@   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  �@   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �4   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �d   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �d   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �d   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �d   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               �A   AO  20111130143448  20190522121828  1727_5046_204                   2C  D   APEX                            2143                            040306                          846 @�8��1   @�:m�?�@7_|�hs�c�=p��
1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @333@�  @�  A��A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  BffB   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4�C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D"��D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9�fD:  D:� D;  D;�fD<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Ddy�Dd��Dey�Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dy� D�#3D�vfD��fD���D�  D�Y�D���D��3D�#3D�` D��fD�� D���D�33Dڃ3D�� D�fD�<�D�` D�vf11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @@  @�ff@�ffA��A#33AC33Ac33A���A���A���A���A���Aљ�AᙚA�B ��B��B��B33B ��B(��B0��B8��B@��BH��BP��BX��B`��Bh��Bp��Bx��B�ffB�33B�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffC 33C33C33C33C33C
33C33C33C33C33C33C33C33C33C33C33C 33C"33C$33C&33C(33C*33C,33C.33C033C233C4L�C633C833C:33C<33C>33C@33CB33CD33CF33CH33CJ33CL33CN33CP33CR33CT33CV33CX33CZ33C\33C^33C`33Cb33Cd33Cf33Ch33Cj33Cl33Cn33Cp33Cr33Ct33Cv33Cx33Cz33C|33C~33C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�&fC��C��C��C��C��C��C��C��C��C��C�&fC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�&fC��C��C��C�&fC��C��C��C��C��C��C��C��C��C��D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#fD#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9�3D:�D:��D;�D;�3D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd�fDefDe�fDf�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dy��D�)�D�|�D���D��3D�&fD�` D��3D��D�)�D�ffD���D��fD�3D�9�Dډ�D��fD��D�C3D�ffD�|�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�ĜA���A�ƨA�ȴA���A���A���A�ȴA�ȴA�ĜA�ƨA�ȴA�ȴA�ȴA���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A�ȴA�ƨA�ƨA�ĜA�ĜA�ĜA�ĜA�A��wA��9A��FA��FA��RA��RA��RA��FA��FA��FA��FA��FA��A���A��hA��DA��+A��A�v�A�ffA�`BA��A���A��mA�ĜA��`A�\)A�dZA�ĜA�%A���A���A�ĜA�dZA�VA�bNA�Q�A�\)A��FA���A��A�~�A��/A�  A���A�p�A�XA�E�A��A�hsA�9XA��A���A��yA�M�A���A�$�A�oA�p�A��;A�A��+A��A���A��`A��A�;dA��^A���A���A�&�A�p�A��#A�hsA��A���A��DA�VA���A��-A���A���A��A��`A�PA{O�Ay��Aw�;Ar��Aq
=ApE�Ao�Am�mAl�/Al �Ak��Ai%AhbAg`BAd�`AaA_XA]�mA[dZAY;dAXr�AV��AU�hATbAR�jAQ�-APĜANffAM�#AM��AMl�AK��AJQ�AIG�AG��AF�/AF1'AEt�ADVAC
=AB5?A?t�A>Q�A=��A;�mA:�A:jA9��A8�+A6��A61A5"�A3�;A3"�A1K�A0ffA/ƨA.��A,��A*�RA)x�A(��A(��A(�A(n�A(A�A'�;A'G�A&ȴA&�+A%�-A$�`A#;dA"��A!7LA bNA�jAC�AbNA$�A��A�-Al�A�RA=qA�A%A^5AAXA7LA�A��AbA�A�uA�mA��A��A��AAoAJA�A�uA/A
�DA
1'A	�FA	&�A��AQ�A��A�A��A��A�
A�Ap�A ��A   @�$�@��@��y@���@�O�@���@�z�@��m@�;d@��@���@�bN@�33@�J@�%@�bN@��@�ff@�X@��@���@�n�@���@���@�-@�9@�9X@߾w@ާ�@���@�|�@�-@�?}@ش9@�|�@�~�@�X@�j@ӝ�@ҏ\@Ь@�=q@�X@�C�@ɉ7@�r�@š�@�1@ÍP@�@�@�J@���@�`B@�/@��@�X@�ff@�?}@�ƨ@�j@���@�~�@�(�@���@��@�z�@�  @�  @���@��!@�O�@�%@�Ĝ@�  @�;d@��@�O�@�I�@���@���@��-@���@��h@�p�@�&�@���@��@��/@��@�A�@�t�@��!@�@�"�@�+@���@�o@�o@�\)@�
=@���@�=q@���@�+@�33@�"�@�
=@��H@��@��T@��#@���@��h@�O�@�7L@�V@��@�z�@�Z@�1'@�1@���@��;@�ƨ@��@��@��R@�n�@�$�@���@�O�@��@���@��/@���@��@���@��@�bN@��@��F@�t�@�@�ȴ@��R@�E�@�-@��@���@��#@�`B@�%@���@���@�Z@�9X@�  @���@�t�@�S�@�K�@�ȴ@�n�@�E�@�@��@�`B@�V@�Ĝ@�bN@��@���@�+@�V@�5?@�{@��@��^@��h@��@���@�1'@��
@��P@�l�@�\)@�\)@�;d@�"�@��@���@���@�n�@�M�@��@�J@�J@���@��7@�&�@��@���@���@���@�Ĝ@��u@�z�@�(�@�  @���@��@��
@���@�dZ@�+@�33@�"�@�o@���@�n�@��#@���@���@�/@��@�Q�@��@��
@���@�C�@�o@��@��H@�v�@�=q@�ff@��+@��!@�~�@�M�@�J@�@��-@�$�@��m@}/@s��@i��@\�j@U�@P��@G\)@@r�@9x�@5@.��@(bN@"M�@�D@K�@��@l�@��@bN11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�ĜA���A�ƨA�ȴA���A���A���A�ȴA�ȴA�ĜA�ƨA�ȴA�ȴA�ȴA���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A�ȴA�ƨA�ƨA�ĜA�ĜA�ĜA�ĜA�A��wA��9A��FA��FA��RA��RA��RA��FA��FA��FA��FA��FA��A���A��hA��DA��+A��A�v�A�ffA�`BA��A���A��mA�ĜA��`A�\)A�dZA�ĜA�%A���A���A�ĜA�dZA�VA�bNA�Q�A�\)A��FA���A��A�~�A��/A�  A���A�p�A�XA�E�A��A�hsA�9XA��A���A��yA�M�A���A�$�A�oA�p�A��;A�A��+A��A���A��`A��A�;dA��^A���A���A�&�A�p�A��#A�hsA��A���A��DA�VA���A��-A���A���A��A��`A�PA{O�Ay��Aw�;Ar��Aq
=ApE�Ao�Am�mAl�/Al �Ak��Ai%AhbAg`BAd�`AaA_XA]�mA[dZAY;dAXr�AV��AU�hATbAR�jAQ�-APĜANffAM�#AM��AMl�AK��AJQ�AIG�AG��AF�/AF1'AEt�ADVAC
=AB5?A?t�A>Q�A=��A;�mA:�A:jA9��A8�+A6��A61A5"�A3�;A3"�A1K�A0ffA/ƨA.��A,��A*�RA)x�A(��A(��A(�A(n�A(A�A'�;A'G�A&ȴA&�+A%�-A$�`A#;dA"��A!7LA bNA�jAC�AbNA$�A��A�-Al�A�RA=qA�A%A^5AAXA7LA�A��AbA�A�uA�mA��A��A��AAoAJA�A�uA/A
�DA
1'A	�FA	&�A��AQ�A��A�A��A��A�
A�Ap�A ��A   @�$�@��@��y@���@�O�@���@�z�@��m@�;d@��@���@�bN@�33@�J@�%@�bN@��@�ff@�X@��@���@�n�@���@���@�-@�9@�9X@߾w@ާ�@���@�|�@�-@�?}@ش9@�|�@�~�@�X@�j@ӝ�@ҏ\@Ь@�=q@�X@�C�@ɉ7@�r�@š�@�1@ÍP@�@�@�J@���@�`B@�/@��@�X@�ff@�?}@�ƨ@�j@���@�~�@�(�@���@��@�z�@�  @�  @���@��!@�O�@�%@�Ĝ@�  @�;d@��@�O�@�I�@���@���@��-@���@��h@�p�@�&�@���@��@��/@��@�A�@�t�@��!@�@�"�@�+@���@�o@�o@�\)@�
=@���@�=q@���@�+@�33@�"�@�
=@��H@��@��T@��#@���@��h@�O�@�7L@�V@��@�z�@�Z@�1'@�1@���@��;@�ƨ@��@��@��R@�n�@�$�@���@�O�@��@���@��/@���@��@���@��@�bN@��@��F@�t�@�@�ȴ@��R@�E�@�-@��@���@��#@�`B@�%@���@���@�Z@�9X@�  @���@�t�@�S�@�K�@�ȴ@�n�@�E�@�@��@�`B@�V@�Ĝ@�bN@��@���@�+@�V@�5?@�{@��@��^@��h@��@���@�1'@��
@��P@�l�@�\)@�\)@�;d@�"�@��@���@���@�n�@�M�@��@�J@�J@���@��7@�&�@��@���@���@���@�Ĝ@��u@�z�@�(�@�  @���@��@��
@���@�dZ@�+@�33@�"�@�o@���@�n�@��#@���@���@�/@��@�Q�@��@��
@���@�C�@�o@��@��H@�v�@�=q@�ff@��+@��!@�~�@�M�@�J@�@��-@�$�@��m@}/@s��@i��@\�j@U�@P��@G\)@@r�@9x�@5@.��@(bN@"M�@�D@K�@��@l�@��@bN11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB@�B?}B@�BA�BA�BA�BA�BA�BA�BB�BB�BB�BB�BB�BB�BC�BB�BB�BC�BB�BC�BC�BC�BC�BE�BE�BD�BD�BD�BC�BE�BE�BF�BE�BF�BF�BF�BF�BF�BH�BH�BG�BH�BH�BH�BH�BH�BI�BK�BM�BO�BVB^5BffBo�Bp�Bs�Bw�Bw�Br�Bm�BjBe`BJ�B9XB �BoB��B�B��B�wB�LB�'B��B��B�bB�1B{�Bk�Be`B^5BM�BB�B?}B=qB<jB8RB33B'�B�B  B�B�yB�)B�B��BĜB�B��B��B�PB�+B{�Bt�Bn�Be`BR�B33B�B{B
=BB
��B
�sB
��B
�3B
��B
��B
�1B
{�B
u�B
o�B
YB
A�B
7LB
(�B
bB
%B	��B	��B	�B	�B	�mB	�TB	�
B	��B	��B	�jB	�B	��B	��B	�{B	�+B	� B	x�B	s�B	l�B	gmB	aHB	[#B	N�B	N�B	P�B	N�B	E�B	9XB	5?B	.B	)�B	$�B	�B	�B	bB	+B��B�B�B�yB�`B�ZB�BB�B��B��B��BƨBB�RB�3B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�hB�VB�DB�7B�B�%B�B�B�B� B~�B}�B|�Bz�By�Bz�By�By�By�Bx�Bw�Bw�Bv�Bt�Bs�Br�Bp�Bo�Bo�Bn�Bn�Bn�Bk�BiyBjBiyBk�Bk�BhsBhsBffBffBgmBhsBiyBiyBhsBhsBhsBhsBjBk�Bk�Bl�Bl�Bk�BhsBl�Bo�Bo�Bo�Bo�Bo�Bq�Bq�Bq�Bs�Bt�Bs�Br�Br�Bq�Bn�Br�B|�B}�B|�B|�B}�B~�B�B�%B�1B�DB�JB�PB�PB�JB�=B�%B�1B�7B�=B�DB�JB�bB�hB�uB��B��B��B��B��B��B��B��B��B��B�FB�dB�XB�XB�qB�^BB��B��B��B�B�B�B�#B�#B�B�B��B��B��B��B�B�B�B�#B�5B�B�B�B�B�B�B�B�B�B��B��B��B	B	B	
=B	DB	VB	VB	�B	�B	�B	�B	�B	�B	$�B	&�B	&�B	&�B	'�B	)�B	+B	,B	/B	0!B	1'B	33B	49B	6FB	7LB	7LB	9XB	;dB	>wB	A�B	C�B	E�B	H�B	J�B	K�B	L�B	L�B	M�B	M�B	M�B	N�B	O�B	R�B	T�B	T�B	XB	ZB	\)B	]/B	^5B	_;B	_;B	bNB	dZB	dZB	e`B	ffB	ffB	gmB	jB	l�B	m�B	m�B	p�B	s�B	s�B	t�B	u�B	x�B	y�B	x�B	z�B	|�B	~�B	�B	�B	�B	�+B	�+B	�1B	�7B	�JB	�VB	�bB	�hB	�oB	�uB	�uB	�uB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�-B	�?B	�LB	�RB	�XB	�XB	�^B	�^B	�^B	�dB	�dB	�jB	�jB	�qB	�qB	�qB	�qB	��B	ÖB	ǮB	ɺB	��B	��B	ɺB	ɺB	ȴB	��B	�B	��B
1B
\B
bB
�B
�B
+B
49B
;dB
@�B
A�B
G�B
M�B
R�B
\)B
`BB
ffB
iyB
n�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B@�B?}B@�BA�BA�BA�BA�BA�BA�BB�BB�BB�BB�BB�BB�BC�BB�BB�BC�BB�BC�BC�BC�BC�BE�BE�BD�BD�BD�BC�BE�BE�BF�BE�BF�BF�BF�BF�BF�BH�BH�BG�BH�BH�BH�BH�BH�BI�BK�BM�BO�BVB^5BffBo�Bp�Bs�Bw�Bx�Bs�Bm�Bk�BiyBM�B>wB#�B�B��B��B��B��B�XB�?B�B��B�{B�PB�Bn�BiyBcTBO�BC�B?}B=qB=qB<jB:^B.B�BB��B�B�BB�/B��B��B�B��B��B�VB�DB}�Bu�Bq�BjB^5B:^B#�B�BJB%B
��B
�B
�TB
�LB
�B
��B
�JB
}�B
x�B
u�B
dZB
E�B
=qB
7LB
�B
1B
B	��B	��B	�B	�yB	�B	�B	��B	��B	ĜB	�B	��B	��B	��B	�7B	�B	{�B	w�B	p�B	jB	dZB	aHB	P�B	N�B	R�B	S�B	I�B	<jB	9XB	1'B	,B	&�B	"�B	�B	uB	VB	B��B��B�B�mB�fB�TB�5B��B��B��BɺBǮB�dB�?B�'B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB�oB�PB�VB�7B�1B�B�B�B�B�B~�B}�B}�B{�B{�B{�By�By�By�By�Bz�Bx�Bv�Bw�Bu�Bt�Br�Bq�Bq�Bq�Bp�Bo�Bk�Bk�Bk�Bm�Bp�Bl�BjBiyBjBjBjBjBjBk�Bk�Bk�Bl�Bl�Bl�Bl�Bm�Bm�Bl�Bn�Bo�Bo�Bp�Bq�Bq�Bq�Br�Br�Bs�Bu�Bu�Bt�Bt�Bt�Br�Bq�Bt�B}�B~�B~�B~�B�B�B�B�+B�=B�PB�VB�VB�VB�VB�PB�=B�7B�JB�PB�PB�bB�oB�oB�{B��B��B��B��B��B��B��B��B��B��B�?B�jB�^B�jB��B�dBÖB��B��B��B�#B�B�B�#B�)B�B�B��B��B��B��B�B�B�B�#B�;B�B�B�B�B�B�B�B�B�B��B��B��B	B	B	DB	JB	\B	PB	�B	�B	�B	�B	�B	 �B	$�B	&�B	&�B	'�B	'�B	)�B	+B	-B	/B	0!B	1'B	33B	49B	6FB	7LB	8RB	:^B	;dB	?}B	B�B	D�B	E�B	H�B	J�B	K�B	L�B	L�B	M�B	M�B	M�B	N�B	P�B	R�B	VB	T�B	XB	[#B	\)B	]/B	^5B	_;B	`BB	cTB	dZB	dZB	e`B	ffB	ffB	hsB	jB	l�B	m�B	n�B	q�B	s�B	s�B	t�B	v�B	x�B	y�B	y�B	z�B	}�B	� B	�B	�B	�B	�+B	�+B	�1B	�=B	�PB	�\B	�hB	�hB	�oB	�uB	�uB	�uB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�-B	�FB	�RB	�RB	�XB	�^B	�dB	�dB	�dB	�dB	�dB	�qB	�jB	�qB	�qB	�wB	�qB	��B	ÖB	ǮB	ɺB	��B	��B	ɺB	ɺB	ȴB	��B	�B	��B
1B
\B
bB
�B
�B
)�B
49B
;dB
@�B
A�B
G�B
L�B
R�B
\)B
`BB
ffB
iyB
n�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<�o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<#�
<#�
<e`B<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =-0.2 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031447452012010314474520120103144746  AO  ARGQ                                                                        20111130143448  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130143448  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144746  IP                  G�O�G�O�G�O�                