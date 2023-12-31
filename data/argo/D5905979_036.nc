CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-19T17:09:01Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         ZPRIMARY | https://orcid.org/0000-0001-7324-3159 | Matthew Alkire, University of Washington        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7(   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7,   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    70   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7@   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7P   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7`   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7h   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    8   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    8    DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     8$   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8D   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8H   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8L   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8l   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8�   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           8�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  i�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  q�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    y�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �T   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �d   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �h   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �x   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �|   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200619170901  20220204114414  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               $A   AO  7662                            2C  D   APEX                            8312                            080318                          846 @ؑ�e�#1   @ؑ�8�@8�V��c�bM��1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    $A   B   B   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB�CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� DfD� D  D� D��D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!y�D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5�fD6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DOfDO�fDP  DP� DQ  DQ� DRfDR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt��Dy��D�'�D�X�D���D��qD�)�D�W�D��=D��D��D�L{D���D��{D�!HD�K3DځHD���D�3D�V�D�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @vfg@���@�A��A8��AX��Ax��A�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffB33B33B33B33B&33B.33B633B>33BF33BN33BV33B^33Bf33Bn33Bv33B~33B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA�gCC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��3C��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fD c3D �3Dc3D�3Dc3D�Dc3D�3Dc3D��Dc3D�3Dc3D�3Dc3D�3Dc3D�3D	c3D	�3D
c3D
�3Dc3D�3Dc3D�3Dc3D�3Dc3D�3Dc3D�3Dc3D�3Dc3D�3Dc3D�3Dc3D�3Dc3D�3Dc3D�3Dc3D�3Di�D�3Dc3D�3Dc3D�3Dc3D�3Dc3D�3Dc3D�3Dc3D�3Dc3D�3Dc3D�3D c3D �3D!\�D!�3D"c3D"�3D#c3D#�3D$c3D$�3D%c3D%�3D&c3D&�3D'c3D'�3D(c3D(�3D)c3D)�3D*c3D*�3D+c3D+�3D,c3D,�3D-c3D-�3D.c3D.�3D/c3D/�3D0c3D0�3D1c3D1�3D2c3D2�3D3c3D3�3D4c3D4�3D5i�D5�3D6c3D6�3D7c3D7�3D8c3D8�3D9c3D9�3D:c3D:�3D;c3D;�3D<c3D<�3D=c3D=�3D>c3D>�3D?c3D?�3D@c3D@�3DAc3DA�3DBc3DB�3DCc3DC�3DDc3DD�3DEc3DE�3DFc3DF�3DGc3DG�3DHc3DH�3DIc3DI�3DJc3DJ�3DKc3DK�3DLc3DL�3DMc3DM�3DNc3DN�DOi�DO�3DPc3DP�3DQc3DQ�DRc3DR�3DSc3DS�3DTc3DT�3DUc3DU�3DVc3DV�3DWc3DW�3DXc3DX�3DYc3DY�3DZc3DZ�3D[c3D[�3D\c3D\�3D]c3D]�3D^c3D^�3D_c3D_�3D`c3D`�3Dac3Da�3Dbc3Db�3Dcc3Dc�3Ddc3Dd�3Dec3De�3Dfc3Df�3Dgc3Dg�3Dhc3Dh�3Dic3Di�3Djc3Dj�3Dkc3Dk�3Dlc3Dl�3Dmc3Dm�3Dnc3Dn�3Doc3Do�3Dpc3Dp�3Dqc3Dq�3Drc3Dr�3Dsc3Ds�3Dtc3Dt� Dy�D�HD�J�D���D��D�4D�IHD���D��)D�gD�>D��qD��D��D�<�D�r�D��gD��D�HRD�HD��4111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A�  A�&�A�&�A�+A�&�A�(�A�$�A�$�A�"�A�$�A�(�A�+A�/A�/A�-A�-A�1'A�1'A�/A�-A�(�A�&�A�+A�(�A�33A̸RA�\)A�Q�A��A���A��7A�M�A�ĜA�?}A�l�A��/A���A�VA��7A��\A��A�z�A��DA��yA�/A�ZA�-A�ȴA�%A�C�A�Q�A�A�A��jA�M�A��DA��\A��A�XA�p�A�ZA�/A�oA��A��
A���A�XA���A�|�A���A���A��A�K�A�ƨA�ffA��A���A�l�A�%A���A��A���A�"�A�t�A�+A���A��A�A��uA��A��hA�A��A���A�?}A��\A�O�A�(�A��A��A��;A��A�n�A��A��mA�p�A�JA���A�K�A��A��A�M�A�JA�33A�/A��A���A�ĜA��PA��A}7LAy�;Av(�At^5As\)Ar�Ar1'Ao��An�!Am�wAl��Al�Ak
=Ai�;Af(�AbĜAa`BA`��A_�FA\�HA[dZAXĜAXVAW�AUC�ASt�AQAP��ANVAM�ALE�AK��AJ��AE�-AB  AAoA@A�A>~�A=�
A=K�A;�-A:��A9��A9S�A85?A6ĜA5�^A4v�A3�;A2�A1`BA0�DA.�A-+A+�A*��A*n�A)��A((�A&�`A%�
A#��A#O�A"�A"�A!?}A $�A?}AA��A�A��A�uA�Ap�A�;A�A�/A$�AȴA��A�\A�DAM�A��A%A�RA  AXA�AA`BA7LAĜA�wA�jA1AdZA
�HA
  A�RA%A�+A�A��AO�A
=A��AO�A�@�+@���@�9X@��@�|�@�ȴ@���@�&�@�K�@���@�C�@�ff@�&�@���@�P@�\@�h@�V@��@�Q�@�(�@�  @�P@��@�(�@�\)@�@�=q@�7@��@��@�33@�G�@۶F@�ȴ@��@�|�@�@�(�@��@�@�%@��@��@͡�@�/@���@̼j@̛�@�1'@�  @˝�@�+@��@ȓu@��@�33@���@ċD@�ȴ@�ff@��@���@��@��m@��R@�X@�r�@��@�ff@��h@�?}@���@�I�@��F@��@�^5@�x�@��u@�Q�@�(�@���@�ƨ@�+@�{@�V@���@� �@��
@��!@���@���@�x�@��@���@��@�9X@��P@���@��-@��/@�j@�Q�@�bN@���@�Ĝ@�Z@��@��@��@��@���@��\@�E�@��-@�`B@��w@�ȴ@��@�t�@���@�p�@��@��;@��/@�V@�Ĝ@��@�bN@�A�@�r�@�Q�@�1@��w@�dZ@�l�@��y@��R@���@��@�X@��/@��@��@��@�`B@�V@���@�x�@�X@�1'@���@�|�@��@��P@��@�@�v�@��#@�@���@��^@��7@���@���@��@�`B@���@���@�I�@�1'@���@�@�n�@�5?@�J@���@���@��@�1'@�t�@�33@�@�o@�+@�dZ@�K�@��@�
=@��#@�X@��@��@���@��j@��@�r�@�r�@��u@��u@�j@�bN@�Z@�j@�j@�Z@�I�@�1'@��@��@�1@��@���@���@�dZ@�;d@�33@�+@��@�ff@�$�@�@���@��@��@�O�@���@�9X@�\)@�~�@�ff@�^5@�-@��-@�`B@�/@�7L@�?}@�/@���@��@��9@��D@�bN@�Q�@�Q�@���@���@���@�t�@�\)@�\)@�\)@�dZ@�dZ@�dZ@�S�@�33@�
=@�;d@��@x�@q�@h��@^�y@U[W@M��@E�@A*0@;��@5�@0�z@+ƨ@&�@�&@($@&@�:@9�@y�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A���A�  A�&�A�&�A�+A�&�A�(�A�$�A�$�A�"�A�$�A�(�A�+A�/A�/A�-A�-A�1'A�1'A�/A�-A�(�A�&�A�+A�(�A�33A̸RA�\)A�Q�A��A���A��7A�M�A�ĜA�?}A�l�A��/A���A�VA��7A��\A��A�z�A��DA��yA�/A�ZA�-A�ȴA�%A�C�A�Q�A�A�A��jA�M�A��DA��\A��A�XA�p�A�ZA�/A�oA��A��
A���A�XA���A�|�A���A���A��A�K�A�ƨA�ffA��A���A�l�A�%A���A��A���A�"�A�t�A�+A���A��A�A��uA��A��hA�A��A���A�?}A��\A�O�A�(�A��A��A��;A��A�n�A��A��mA�p�A�JA���A�K�A��A��A�M�A�JA�33A�/A��A���A�ĜA��PA��A}7LAy�;Av(�At^5As\)Ar�Ar1'Ao��An�!Am�wAl��Al�Ak
=Ai�;Af(�AbĜAa`BA`��A_�FA\�HA[dZAXĜAXVAW�AUC�ASt�AQAP��ANVAM�ALE�AK��AJ��AE�-AB  AAoA@A�A>~�A=�
A=K�A;�-A:��A9��A9S�A85?A6ĜA5�^A4v�A3�;A2�A1`BA0�DA.�A-+A+�A*��A*n�A)��A((�A&�`A%�
A#��A#O�A"�A"�A!?}A $�A?}AA��A�A��A�uA�Ap�A�;A�A�/A$�AȴA��A�\A�DAM�A��A%A�RA  AXA�AA`BA7LAĜA�wA�jA1AdZA
�HA
  A�RA%A�+A�A��AO�A
=A��AO�A�@�+@���@�9X@��@�|�@�ȴ@���@�&�@�K�@���@�C�@�ff@�&�@���@�P@�\@�h@�V@��@�Q�@�(�@�  @�P@��@�(�@�\)@�@�=q@�7@��@��@�33@�G�@۶F@�ȴ@��@�|�@�@�(�@��@�@�%@��@��@͡�@�/@���@̼j@̛�@�1'@�  @˝�@�+@��@ȓu@��@�33@���@ċD@�ȴ@�ff@��@���@��@��m@��R@�X@�r�@��@�ff@��h@�?}@���@�I�@��F@��@�^5@�x�@��u@�Q�@�(�@���@�ƨ@�+@�{@�V@���@� �@��
@��!@���@���@�x�@��@���@��@�9X@��P@���@��-@��/@�j@�Q�@�bN@���@�Ĝ@�Z@��@��@��@��@���@��\@�E�@��-@�`B@��w@�ȴ@��@�t�@���@�p�@��@��;@��/@�V@�Ĝ@��@�bN@�A�@�r�@�Q�@�1@��w@�dZ@�l�@��y@��R@���@��@�X@��/@��@��@��@�`B@�V@���@�x�@�X@�1'@���@�|�@��@��P@��@�@�v�@��#@�@���@��^@��7@���@���@��@�`B@���@���@�I�@�1'@���@�@�n�@�5?@�J@���@���@��@�1'@�t�@�33@�@�o@�+@�dZ@�K�@��@�
=@��#@�X@��@��@���@��j@��@�r�@�r�@��u@��u@�j@�bN@�Z@�j@�j@�Z@�I�@�1'@��@��@�1@��@���@���@�dZ@�;d@�33@�+@��@�ff@�$�@�@���@��@��@�O�@���@�9X@�\)@�~�@�ff@�^5@�-@��-@�`B@�/@�7L@�?}@�/@���@��@��9@��D@�bN@�Q�@�Q�@���@���@���@�t�@�\)@�\)@�\)@�dZ@�dZ@�dZ@�S�@�33G�O�@�;d@��@x�@q�@h��@^�y@U[W@M��@E�@A*0@;��@5�@0�z@+ƨ@&�@�&@($@&@�:@9�@y�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB<jB<jB;dB;dB;dB;dB;dB<jB;dB<jB<jB;dB;dB;dB<jB<jB<jB;dB<jB<jB<jB<jB<jB<jB<jB;dB:^BB�BR�BN�BQ�BT�BVBVBXBYB[#BZB[#B]/B`BBdZBe`Bl�Bm�Bq�Br�Br�Bs�Bt�Bt�Bx�B{�B|�B|�B|�B|�B|�B~�B~�Bz�Bz�Bz�Bz�By�By�Bz�By�Bx�Bu�Bs�Bs�Bp�Bl�BiyBcTB`BBT�BK�BG�B7LB,B�BuB\B��B�B�/B�B��B��B��B�9B��B��B��B��B��B�uB�PB�Bp�BhsBcTBO�B7LB,B#�BVB
�B
��B
ȴB
ÖB
�?B
��B
�=B
�B
y�B
s�B
iyB
L�B
6FB
�B
\B
B	��B	��B	�sB	�)B	��B	��B	ÖB	�qB	�-B	��B	�B	x�B	v�B	q�B	`BB	VB	F�B	C�B	C�B	9XB	.B	�B	uB	B	B��B�B�fB��B�B�B�B��B��B��B��B�oB�\B�\B�VB�JB�1B�B}�B{�Bs�Bm�BjBffBhsBiyBn�Bo�Bk�Be`BcTB_;B]/B]/B]/B]/B^5B^5BZBYBW
BW
BS�BQ�BO�BN�BJ�BI�BJ�BF�BE�BE�BD�BD�BD�BB�BA�BB�B@�B?}B@�B>wB?}B<jB<jB=qB<jB?}B?}B;dB;dB7LB6FB6FB5?B49B49B49B9XB8RB49B49B33B49B33B49B49B49B49B5?B6FB5?B6FB6FB6FB5?B5?B8RB7LB7LB7LB7LB6FB8RB8RB8RB8RB8RB9XB9XB9XB:^B<jB<jB<jB>wBA�BA�BD�BE�BE�BF�BK�BL�BM�BN�BO�BO�BO�BP�BP�BQ�BQ�BT�BW
BXBZB]/BaHBe`Be`BffBffBhsBjBn�Bq�Bs�Bu�Bz�B|�B}�B~�B�B�B�%B�1B�DB�\B�\B�bB�hB�hB�uB��B��B��B��B��B��B��B��B��B��B��B�B�B�B�9B�LB�XB�dB�dB�jB�wBBĜBÖBŢBȴB��B�B�B�#B�HB�;B�BB�BB�sB�B�B�B��B	B	DB	uB	{B	�B	�B	�B	�B	$�B	+B	.B	-B	.B	/B	49B	6FB	8RB	7LB	7LB	6FB	5?B	0!B	/B	5?B	7LB	<jB	B�B	B�B	G�B	L�B	P�B	Q�B	R�B	T�B	T�B	T�B	T�B	VB	ZB	]/B	`BB	aHB	bNB	dZB	dZB	dZB	e`B	dZB	gmB	k�B	k�B	jB	jB	iyB	hsB	gmB	ffB	ffB	gmB	hsB	jB	l�B	o�B	q�B	r�B	w�B	y�B	{�B	|�B	}�B	� B	�B	�B	�B	�B	�%B	�+B	�+B	�7B	�DB	�JB	�PB	�bB	�hB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�'B	�-B	�'B	�'B	�3B	�LB	�FB	�FB	�LB	�LB	�RB	�^B	�qB	�qB	�wB	�}B	�}B	��B	ÖB	ƨB	ǮB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�]B	�|B
 �B
	B
NB

B
#nB
+B
6�B
=�B
B�B
I�B
N�B
TB
XEB
]B
dB
h$B
kB
pUB
t9111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B3�B3�B2�B2�B2�B2�B2�B3�B2�B3�B3�B2�B2�B2�B3�B3�B3�B2�B3�B3�B3�B3�B3�B3�B3�B2�B1�B:BJ{BFcBIvBL�BM�BM�BO�BP�BR�BQ�BR�BT�BW�B[�B\�BdBeBi5Bj;Bj;BkABlGBlGBp`BssBtzBtzBtzBtzBtzBv�Bv�BrnBrnBroBroBqiBqiBroBqiBpcBmQBkEBkEBh3BdBa	BZ�BW�BL�BCYB?@B.�B#�BNBB�B��B�8B��BѸB̚B�dB�&B��B��B�eB�AB�4B�(B�B��Bx�BhHB`BZ�BG�B.�B#�B�BB
�GB
ʣB
�fB
�HB
��B
�gB
��B
{�B
q�B
koB
a3B
D�B
.B
lB
B	��B	��B	�B	�8B	��B	ʸB	B	�^B	�9B	��B	��B	{�B	p�B	n�B	iyB	XB	M�B	>{B	;iB	;iB	1,B	%�B	|B	LB��B��B�B�_B�AB�gB��B��B��B��B��B��B�|B�RB�?B�?B�:B�.B�B}Bu�Bs�Bk�BexBbgB^NB`[BaaBf�Bg�BcnB]IB[>BW%BUBUBUBUBV BV BR	BQBN�BN�BK�BI�BG�BF�BB�BA�BB�B>�B=�B=�B<�B<�B<�B:~B9xB:~B8rB7mB8sB6gB7mB4ZB4ZB5bB4[B7nB7nB3UB3UB/>B.8B.8B-1B,,B,,B,,B1KB0EB,-B,-B+'B,-B+'B,-B,-B,-B,-B-3B.;B-4B.;B.;B.;B-4B-4B0GB/BB/BB/BB/BB.<B0HB0HB0HB0HB0HB1NB1NB1NB2TB4`B4aB4aB6nB9�B9�B<�B=�B=�B>�BC�BD�BE�BF�BG�BG�BG�BH�BH�BI�BI�BL�BOBPBRBU&BY?B]VB]WB^\B^]B`iBbuBf�Bi�Bk�Bm�Br�Bt�Bu�Bv�Bx�B{B~B�'B�9B�QB�QB�WB�]B�]B�jB��B��B��B��B��B��B��B��B��B��B��B��B�B�B�-B�@B�LB�XB�XB�^B�kB��B��B��B��B��B��B��B�B�B�:B�-B�4B�4B�dB�B�B�|B�B��B	3B	cB	iB	�B	�B	�B	�B	�B	"�B	&B	$�B	&B	'B	,&B	.2B	0>B	/9B	/9B	.3B	-,B	(B	'B	-,B	/9B	4WB	:{B	:{B	?�B	D�B	H�B	I�B	J�B	L�B	L�B	L�B	L�B	M�B	RB	UB	X-B	Y3B	Z9B	\DB	\DB	\EB	]JB	\EB	_WB	coB	coB	biB	biB	acB	`^B	_XB	^QB	^QB	_XB	`^B	bjB	dvB	g�B	i�B	j�B	o�B	q�B	s�B	t�B	u�B	w�B	y�B	y�B	z�B	}B	~B	B	B	� B	�-B	�3B	�9B	�JB	�PB	�]B	�cB	�iB	�iB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�3B	�-B	�-B	�3B	�3B	�9B	�DB	�WB	�WB	�]B	�cB	�cB	�oB	�|B	��B	��B	��B	��B	��B	ìB	ìB	ĲB	ŸB	ƾB	��B	��G�O�B	�AB	�_B	��B
 B
	0B
�B
PB
"�B
.�B
5�B
:�B
A�B
FjB
K�B
P%B
T�B
[�B
`B
b�B
h3B
l111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.45 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9998(+/-0.0001), vertically averaged dS =-0.008(+/-0.002) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144142022020411441420220204114414  AO  ARCAADJP                                                                    20200619170901    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200619170901  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200619170901  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114414  IP                  G�O�G�O�G�O�                