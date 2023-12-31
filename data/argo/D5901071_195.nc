CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:46Z UW 3.1 conversion   
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
_FillValue                 �  A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Kt   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  Mp   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  U\   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]H   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  _D   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  g0   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  i,   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  q   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  y   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  {    PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �0   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �4   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �8   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �<   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �@   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               �A   AO  20111130143406  20190522121828  1727_5046_195                   2C  D   APEX                            2143                            040306                          846 @��bk  1   @����@6�1&�y�c�-V1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�  @�  A   A!��A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B���B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Cg�fCj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,�fD-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DGfDG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DPy�DP��DQy�DQ��DR� DS  DS� DTfDT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� D`��Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dy� D��D�l�D��fD�� D�&fD�l�D���D��3D�33D�c3D���D��fD�0 D�s3Dڜ�D��fD�&fD�p D�3D��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�ff@�ffA33A$��AC33Ac33A���A���A���A���A���Aљ�AᙚA�B ��B��B��B��B ��B(��B0��B8��B@��BH��BP��BX��B`��Bh��Bp��Bx��B�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB���B�ffB�ffB�ffB�ffB�ffB�ffB�ffBԙ�Bؙ�B�ffB�ffB�ffB�ffB�ffB�ffB�ffB�33B�ffC 33C33C33C33C33C
33C33C33C33C33C33C33C33C33C33C33C 33C"33C$33C&33C(33C*33C,33C.33C033C233C433C633C833C:33C<33C>33C@33CB33CD33CF33CH33CJ33CL33CN33CP33CR33CT33CV33CX33CZ33C\33C^33C`33Cb33Cd33Cf33Ch�Cj33Cl33Cn33Cp33Cr33Ct33Cv33Cx33Cz33C|33C~33C�&fC�&fC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�&fC��C��C�&fC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�&fC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�&fC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D3D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,�3D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG3DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP�fDQfDQ�fDRfDR��DS�DS��DT3DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��DafDa��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dy��D�  D�s3D���D��fD�,�D�s3D��3D��D�9�D�i�D�� D���D�6fD�y�Dڣ3D���D�,�D�vfD�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�r�A�r�A�r�A�r�A�n�A�t�A�v�A�x�A�x�A�v�A�z�A�z�A�v�A�z�A�|�A�p�A�z�A�|�A�z�A�|�A�x�A��A��jA�dZA�/A�JA���A���A��!A��hA�r�A�hsA�ZA�XA�;dA�"�A�oA�1A�%A�%A�%A�A��yA�dZA��`A�hsA�\)A�1'A�|�A���A�G�A���A�dZA�r�A��/A��^A�5?A���A���A��\A�33A�VA�O�A�VA�  A�r�A���A��yA�~�A�=qA��A��;A�5?A���A��A�ffA��PA��DA��RA�dZA��+A�?}A��A��yA�A��!A���A�Q�A���A�7LA���A�n�A���A��A�x�A�x�A��\A�dZA�bNA��^A��A�-A��A�ffA��^A��FA���A�=qA�{A��A�E�At�A~v�A}�hA|�A|��A{Ax�!Au��Ap�RAm%Aj�DAjbAi��AiAf�A^A�A\^5AY��AVE�AS�AQC�APr�AP=qAP$�AO�AOl�AN�AN-ALE�AI�AI
=AH=qAG��AF�AFv�AEx�ADȴAD�ADn�AD �ACp�AB��AB5?AA��A@�!A>��A:�DA9`BA8�A8z�A8Q�A8bA7�A7K�A6ȴA5��A3��A1�hA0jA/t�A-��A,�!A,=qA+�A)dZA(1'A'�^A%��A%
=A#�A!��A�`A�A$�A�A�TA��A��Av�A-A  AƨA�hA��A��A~�AM�A$�AJAA��A�Ap�A��AJA�hA9XA��AAv�A��A�DA$�AO�A
�9A
n�A	��A��A��A�uA�AhsA�mAG�A  �@�l�@�-@��#@��;@�ȴ@��@�7@�M�@��@�K�@��T@�G�@�%@�b@�+@�%@��;@��@�`B@�j@���@�;d@ޟ�@�^5@�E�@݁@��@ڸR@�-@�$�@��@ٺ^@���@ׅ@֟�@��/@�v�@��@љ�@�j@ύP@�$�@Ͳ-@�?}@�r�@���@�&�@��@ǍP@�"�@�n�@§�@���@�%@��D@�b@�K�@��H@�V@�$�@�@��@�x�@��D@��y@�7L@���@�Ĝ@��D@�r�@�A�@���@�;d@��H@���@�Q�@��;@��@�5?@��@��@�|�@�t�@�\)@�+@���@��y@��@�ȴ@�-@�@�&�@���@�9X@�ƨ@��@�K�@�"�@��H@�^5@�E�@�=q@�E�@�=q@�$�@��@��@��@�@�x�@��@��@�1'@�t�@��@���@�ff@�=q@�5?@��@��@��#@���@��-@��h@��7@��h@��7@�p�@��@��/@�r�@�ƨ@��P@�dZ@�;d@�@��y@���@��^@�p�@�Ĝ@��@�A�@�  @�@���@�x�@��@�b@��w@�@���@�~�@�E�@�5?@��@���@��-@�x�@�G�@�7L@��@��@�%@���@���@�z�@� �@��
@���@�;d@�"�@��y@��\@�-@��#@���@��7@�G�@��@���@�%@���@�V@�/@�?}@�O�@�X@�X@�X@�G�@�/@��@��9@�j@��m@��F@�dZ@�;d@�dZ@�dZ@�+@���@���@���@��+@�ff@�-@���@��#@���@��-@���@�p�@�&�@��@��`@�Ĝ@��j@��@���@���@��D@�z�@�r�@�bN@�A�@���@�33@�@��y@��y@���@��+@�{@��@��^@�X@�7L@�/@��@��@��@��@���@���@�%@��/@��j@��@���@�1'@�  @��w@��@�|�@�o@��\@�$�@�33@�r�@� �@y&�@n�+@c��@Wl�@NE�@G�;@B��@<z�@6$�@01'@*~�@&v�@"�@��@?}@G�@{@�`111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�r�A�r�A�r�A�r�A�n�A�t�A�v�A�x�A�x�A�v�A�z�A�z�A�v�A�z�A�|�A�p�A�z�A�|�A�z�A�|�A�x�A��A��jA�dZA�/A�JA���A���A��!A��hA�r�A�hsA�ZA�XA�;dA�"�A�oA�1A�%A�%A�%A�A��yA�dZA��`A�hsA�\)A�1'A�|�A���A�G�A���A�dZA�r�A��/A��^A�5?A���A���A��\A�33A�VA�O�A�VA�  A�r�A���A��yA�~�A�=qA��A��;A�5?A���A��A�ffA��PA��DA��RA�dZA��+A�?}A��A��yA�A��!A���A�Q�A���A�7LA���A�n�A���A��A�x�A�x�A��\A�dZA�bNA��^A��A�-A��A�ffA��^A��FA���A�=qA�{A��A�E�At�A~v�A}�hA|�A|��A{Ax�!Au��Ap�RAm%Aj�DAjbAi��AiAf�A^A�A\^5AY��AVE�AS�AQC�APr�AP=qAP$�AO�AOl�AN�AN-ALE�AI�AI
=AH=qAG��AF�AFv�AEx�ADȴAD�ADn�AD �ACp�AB��AB5?AA��A@�!A>��A:�DA9`BA8�A8z�A8Q�A8bA7�A7K�A6ȴA5��A3��A1�hA0jA/t�A-��A,�!A,=qA+�A)dZA(1'A'�^A%��A%
=A#�A!��A�`A�A$�A�A�TA��A��Av�A-A  AƨA�hA��A��A~�AM�A$�AJAA��A�Ap�A��AJA�hA9XA��AAv�A��A�DA$�AO�A
�9A
n�A	��A��A��A�uA�AhsA�mAG�A  �@�l�@�-@��#@��;@�ȴ@��@�7@�M�@��@�K�@��T@�G�@�%@�b@�+@�%@��;@��@�`B@�j@���@�;d@ޟ�@�^5@�E�@݁@��@ڸR@�-@�$�@��@ٺ^@���@ׅ@֟�@��/@�v�@��@љ�@�j@ύP@�$�@Ͳ-@�?}@�r�@���@�&�@��@ǍP@�"�@�n�@§�@���@�%@��D@�b@�K�@��H@�V@�$�@�@��@�x�@��D@��y@�7L@���@�Ĝ@��D@�r�@�A�@���@�;d@��H@���@�Q�@��;@��@�5?@��@��@�|�@�t�@�\)@�+@���@��y@��@�ȴ@�-@�@�&�@���@�9X@�ƨ@��@�K�@�"�@��H@�^5@�E�@�=q@�E�@�=q@�$�@��@��@��@�@�x�@��@��@�1'@�t�@��@���@�ff@�=q@�5?@��@��@��#@���@��-@��h@��7@��h@��7@�p�@��@��/@�r�@�ƨ@��P@�dZ@�;d@�@��y@���@��^@�p�@�Ĝ@��@�A�@�  @�@���@�x�@��@�b@��w@�@���@�~�@�E�@�5?@��@���@��-@�x�@�G�@�7L@��@��@�%@���@���@�z�@� �@��
@���@�;d@�"�@��y@��\@�-@��#@���@��7@�G�@��@���@�%@���@�V@�/@�?}@�O�@�X@�X@�X@�G�@�/@��@��9@�j@��m@��F@�dZ@�;d@�dZ@�dZ@�+@���@���@���@��+@�ff@�-@���@��#@���@��-@���@�p�@�&�@��@��`@�Ĝ@��j@��@���@���@��D@�z�@�r�@�bN@�A�@���@�33@�@��y@��y@���@��+@�{@��@��^@�X@�7L@�/@��@��@��@��@���@���@�%@��/@��j@��@���@�1'@�  @��w@��@�|�@�o@��\@�$�@�33@�r�@� �@y&�@n�+@c��@Wl�@NE�@G�;@B��@<z�@6$�@01'@*~�@&v�@"�@��@?}@G�@{@�`111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�
B�#B�5B�HB�fB�yB�B�B�B��B��B��B��B��B��B��B��B�B�sB�/B�B�5B�5B�5B�)B�B��B��B��B��B��BǮB�XB�wB��B�B�#B�#B�B��BȴB�}B�-B��B��B�JB�B�By�BcTBC�B7LB)�BPB��B��B��B��B��B��B��B��B�B�mB�)BɺB�^B�{BdZBN�B/BJB
�B
�fB
��B
��B
�^B
�9B
��B
�PB
u�B
n�B
l�B
hsB
`BB
YB
R�B
N�B
H�B
E�B
=qB
0!B
hB	�B	�5B	��B	��B	ȴB	�}B	�B	�1B	{�B	m�B	`BB	L�B	E�B	B�B	A�B	@�B	>wB	:^B	5?B	.B	'�B	�B	�B	{B	hB	VB	DB	%B	B	B	B	  B��B��B��B�B�yB�/B��B��B��BɺBȴBǮBƨBĜBB�qB�LB�-B�!B�B�B��B��B��B��B��B��B��B��B��B�uB�JB�7B�7B�7B�7B�1B�+B�+B�+B�%B�B�B�B�B�B�B�B�B�B�B�B~�B{�B|�Bz�B{�B{�Bz�Bz�Bz�B}�B|�B}�B|�By�Bt�Bo�BjBgmBiyBbNBZBYBYBXBVBVBVBS�BR�BS�BVBYBVBW
BW
BVBXBVBW
BW
BW
BYBZB[#B^5B_;B_;B_;B`BBaHBk�Bt�Bw�B{�B�B�oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�VB�PB�\B�bB�oB��B��B��B��B��B��B��B��B��B�B�B�B�!B�'B�-B�FB�XB�jBĜB��B��B�B�B�HB�`B�B�B��B��B��B��B��B��B	B	%B		7B	DB	hB	oB	uB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	 �B	#�B	&�B	%�B	&�B	.B	1'B	33B	49B	49B	49B	5?B	5?B	5?B	5?B	6FB	6FB	6FB	6FB	6FB	6FB	8RB	9XB	;dB	A�B	C�B	D�B	E�B	G�B	H�B	J�B	R�B	VB	\)B	^5B	_;B	aHB	dZB	e`B	e`B	gmB	hsB	iyB	o�B	q�B	r�B	t�B	u�B	v�B	w�B	y�B	z�B	|�B	|�B	|�B	|�B	}�B	}�B	}�B	~�B	�B	�%B	�=B	�VB	�VB	�\B	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�3B	�?B	�RB	�XB	�^B	�dB	�qB	�wB	�}B	�}B	��B	��B	B	ÖB	ÖB	ĜB	ŢB	ŢB	ƨB	ƨB	ǮB	ȴB	ȴB	ȴB	ȴB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�
B	�
B	�
B	�B	�B	�B	�B	�#B	�)B	�/B	�5B	�;B	�;B	�5B	�/B	�5B	�BB	�HB	�NB	�NB	�yB	��B
B
DB
oB
�B
%�B
1'B
;dB
>wB
B�B
C�B
I�B
P�B
VB
ZB
`BB
iyB
l�B
q�B
u�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�BB��B��B��B�B�)B�;B�NB�fB�yB�B�B�B��B��B��B��B��B��B��B��B��B�B�NB�5B�NB�HB�HB�5B�B�B��B��B��B��B��B��B��BƨB�B�)B�#B�)B�/B��BǮB�XB�B��B�bB�B�B� Bm�BH�B<jB1'BhB��B��B��B��B��B��B��B��B�B�B�NB��BÖB��BhsBW
B8RB�B
��B
�B
��B
ÖB
�dB
�LB
�-B
��B
x�B
o�B
m�B
k�B
cTB
\)B
T�B
P�B
I�B
H�B
G�B
:^B
 �B
  B	�fB	��B	��B	��B	ȴB	ŢB	�\B	�B	z�B	k�B	R�B	H�B	C�B	A�B	A�B	@�B	<jB	7LB	2-B	.B	�B	�B	�B	uB	\B	VB	1B	B	B	B	B	  B��B��B��B�B�yB��B��B��B��BɺBȴBǮBƨBƨBÖB�wB�FB�3B�-B�B�B�B�B��B��B��B��B��B��B��B�{B�=B�7B�7B�7B�7B�=B�1B�1B�+B�%B�+B�%B�B�%B�%B�B�B�B�B�B�B�B~�B~�B}�B}�B|�B|�B� B~�B� B� B}�B|�Bz�Bu�Bn�BiyBl�BgmB\)B]/BZB[#B]/BYBXBW
BXBXBXB[#BXBXBW
BW
BZBXBYBXBYBZB[#B\)B_;B_;B_;B`BBbNBcTBl�Bt�Bw�B|�B�%B�uB��B��B��B��B��B��B��B��B��B��B��B�B��B��B��B��B��B��B�\B�VB�bB�hB�uB��B��B��B��B��B��B��B��B��B�B�B�B�!B�'B�3B�LB�^B�wBǮB��B�B�B�#B�NB�fB�B�B��B��B��B��B��B��B	B	+B		7B	JB	oB	oB	uB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	!�B	$�B	&�B	&�B	'�B	/B	1'B	33B	49B	49B	49B	5?B	5?B	5?B	5?B	6FB	6FB	6FB	6FB	6FB	7LB	8RB	:^B	<jB	A�B	C�B	D�B	E�B	G�B	H�B	K�B	R�B	W
B	\)B	^5B	_;B	bNB	e`B	ffB	ffB	hsB	hsB	jB	p�B	q�B	r�B	t�B	u�B	v�B	w�B	y�B	z�B	|�B	|�B	|�B	|�B	}�B	}�B	~�B	� B	�B	�%B	�=B	�VB	�VB	�bB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�B	�'B	�3B	�?B	�RB	�XB	�^B	�dB	�qB	�wB	�}B	�}B	��B	��B	B	ÖB	ÖB	ĜB	ŢB	ŢB	ƨB	ƨB	ǮB	ȴB	ȴB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�
B	�
B	�
B	�
B	�
B	�B	�B	�B	�B	�#B	�)B	�/B	�5B	�BB	�;B	�5B	�/B	�5B	�HB	�NB	�TB	�NB	�yB	��B
B

=B
oB
�B
%�B
1'B
;dB
>wB
B�B
C�B
I�B
P�B
VB
ZB
`BB
iyB
l�B
p�B
u�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<u<D��<#�
<#�
<#�
<#�
<#�
<���<#�
<#�
<T��<49X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =-0.2 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031447422012010314474220120103144742  AO  ARGQ                                                                        20111130143406  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130143406  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144742  IP                  G�O�G�O�G�O�                