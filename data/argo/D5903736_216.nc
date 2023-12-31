CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-04-13T22:00:52Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         (Matthew Alkire, University of Washington      @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7,   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  74   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7t   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     88   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8X   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8x   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           8|   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
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
_FillValue                 �  A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  Ux   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]d   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _`   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  gL   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  iH   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  q4   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  y    PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �    SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �    SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �    SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �    HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �L   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �P   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �T   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �X   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �\   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20180413220052  20190604094145  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4051                            2C  D   APEX                            5368                            041511                          846 @�Z轡C�1   @�Z�d�l@5�\(��d�z�G�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @�ff@�  A   A   AA��Aa��A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#fD#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  Dey�Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Ds��Dy�D�fD�G�D�k�D��D��D�<{D���D��qD��D�S3D��{D���D�fD�8RDڄ{D��qD��HD�1�D�[�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��A�\A@(�A`(�A~�\A�G�A�G�A�G�A�G�A�G�A�G�A�G�A�G�B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{D z=D �=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=D	z=D	�=D
z=D
�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=D z=D �=D!z=D!�=D"z=D# �D#z=D#�=D$z=D$�=D%z=D%�=D&z=D&�=D'z=D'�=D(z=D(�=D)z=D)�=D*z=D*�=D+z=D+�=D,z=D,�=D-z=D-�=D.z=D.�=D/z=D/�=D0z=D0�=D1z=D1�=D2z=D2�=D3z=D3�=D4z=D4�=D5z=D5�=D6z=D6�=D7z=D7�=D8z=D8�=D9z=D9�=D:z=D:�=D;z=D;�=D<z=D<�=D=z=D=�=D>z=D>�=D?z=D?�=D@z=D@�=DAz=DA�=DBz=DB�=DCz=DC�=DDz=DD�=DEz=DE�=DFz=DF�=DGz=DG�=DHz=DH�=DIz=DI�=DJz=DJ�=DKz=DK�=DLz=DL�=DMz=DM�=DNz=DN�=DOz=DO�=DPz=DP�=DQz=DQ�=DRz=DR�=DSz=DS�=DTz=DT�=DUz=DU�=DVz=DV�=DWz=DW�=DXz=DX�=DYz=DY�=DZz=DZ�=D[z=D[�=D\z=D\�=D]z=D]�=D^z=D^�=D_z=D_�=D`z=D`�=Daz=Da�=Dbz=Db�=Dcz=Dc�=Ddz=Dd�=Des�De�=Dfz=Df�=Dgz=Dg�=Dhz=Dh�=Diz=Di�=Djz=Dj�=Dkz=Dk�=Dlz=Dl�=Dmz=Dm�=Dnz=Dn�=Doz=Do�=Dpz=Dp�=Dqz=Dq�=Drz=Dr�=Dsz=Ds��Dy�\D��D�D�D�h�D�޹D�	�D�9�D���D���D��D�PRD���D���D��D�5qDځ�D�ҐD��gD�/D�X�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�
=A�1A�1A�JA�JA�VA�JA���A���A�AÝ�A�r�A�\)A�ZA�VA�O�A�;dA���A� �A���A�JA��!A�?}A��/A�ƨA��wA��jA��^A��A�S�A�(�A��A��A��A�v�A�O�A�;dA�&�A��`A�r�A��A��#A�ĜA��RA��A��A�A�A�A�(�A�^5A�  A���A�|�A��A�ƨA��\A�v�A���A�9XA��/A�`BA��-A�1'A�7LA�XA�|�A�ffA�ȴA�A�jA�{A���A���A�hsA�bNA��yA��A�O�A�VA�^5A�
=A�5?A��DA�VA���A��A� �A��wA���A�9XA�&�A��yA�t�A���A���A�A�/A�^5A��A�A�
=A��^A���A��A�t�A�1'A���A�33A~z�A|��A|1A{
=Ay�FAx�`Aw�wAv��At�\Ar��Aq�PAp�yAo�#Anr�AlVAh�`Ad��AbVA^�yA]&�A\�/A\A�AZ��AX1AV�AT9XAQƨAPĜAOO�ALVAIdZAIAG�wAE�AD(�AC%AA�FA@n�A?�PA>I�A=?}A=VA;��A9�A8�DA7��A6��A5`BA49XA3��A2�A2�9A2v�A2JA1��A0��A.��A-��A,�uA,5?A+hsA*�A(�!A&�A&$�A%�;A%33A$�!A$=qA#S�A!�TA�7AZA�A�A|�A{A�A��A��A�A�!A�#A��A��A33Az�A�hA��A�AVA1AG�A
�/A
-A�uAA�-AG�AZAx�A;dAZAG�A j@�S�@���@��`@�l�@��!@���@��/@�"�@�ff@��@���@��@��#@���@@���@�R@���@��@�Q�@��@�9X@�
=@�G�@�Ĝ@�  @��@�ƨ@�t�@�E�@�z�@�dZ@�@��@�9X@ۥ�@�S�@�"�@�ȴ@��@�p�@�%@���@��m@֧�@�@�X@ԓu@ӍP@�J@���@��@�  @ϝ�@�M�@�@�G�@�Ĝ@��
@�ȴ@�E�@��T@ə�@��
@�@���@���@Ł@Å@�V@���@�7L@���@�1@��w@�K�@��@�~�@�hs@�A�@�dZ@���@�^5@�bN@�1@��m@�C�@�-@��h@�&�@��`@��9@�  @���@��y@��@��-@�&�@��9@�1'@�l�@�J@�V@�(�@��@��!@��@�@��H@�ȴ@��+@�ff@�M�@�hs@��j@�Z@�1'@�1@���@�\)@��@�@���@��-@�Q�@��P@�"�@�C�@�l�@���@��w@�|�@�+@��!@�n�@�ff@�V@�E�@�?}@���@�Q�@��@�b@���@��;@���@���@�l�@�33@���@���@�ff@�@���@���@��7@�p�@��@��j@��@�I�@���@���@�|�@�+@��@�@�ȴ@�-@�$�@��@��@���@��9@��@�bN@�Q�@�1'@�  @��;@�ƨ@��@���@�S�@�K�@��@��@���@��@�X@�G�@�?}@�7L@���@��@�9X@�(�@���@��;@���@��F@���@�l�@�K�@�"�@���@���@�~�@�E�@�@�@�x�@�O�@�&�@���@���@�Ĝ@��j@��@��u@�j@���@��F@��P@�t�@�"�@���@�^5@�M�@�=q@�$�@�@��#@���@��7@�X@��@���@���@��j@���@��@�r�@�Q�@�b@��;@��w@�\)@�33@�+@�"�@��@�o@�@��@���@��T@��7@�hs@�&�@���@��9@���@�Q�@�1@�ƨ@��@�|�@��@�"�@�
=@��y@�ȴ@���@�dZ@{��@r�1@lS�@d��@\�.@T�.@NkQ@J��@E�N@?/�@6($@/9�@)@#iD@�@�@�&@[�@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111 A�
=A�1A�1A�JA�JA�VA�JA���A���A�AÝ�A�r�A�\)A�ZA�VA�O�A�;dA���A� �A���A�JA��!A�?}A��/A�ƨA��wA��jA��^A��A�S�A�(�A��A��A��A�v�A�O�A�;dA�&�A��`A�r�A��A��#A�ĜA��RA��A��A�A�A�A�(�A�^5A�  A���A�|�A��A�ƨA��\A�v�A���A�9XA��/A�`BA��-A�1'A�7LA�XA�|�A�ffA�ȴA�A�jA�{A���A���A�hsA�bNA��yA��A�O�A�VA�^5A�
=A�5?A��DA�VA���A��A� �A��wA���A�9XA�&�A��yA�t�A���A���A�A�/A�^5A��A�A�
=A��^A���A��A�t�A�1'A���A�33A~z�A|��A|1A{
=Ay�FAx�`Aw�wAv��At�\Ar��Aq�PAp�yAo�#Anr�AlVAh�`Ad��AbVA^�yA]&�A\�/A\A�AZ��AX1AV�AT9XAQƨAPĜAOO�ALVAIdZAIAG�wAE�AD(�AC%AA�FA@n�A?�PA>I�A=?}A=VA;��A9�A8�DA7��A6��A5`BA49XA3��A2�A2�9A2v�A2JA1��A0��A.��A-��A,�uA,5?A+hsA*�A(�!A&�A&$�A%�;A%33A$�!A$=qA#S�A!�TA�7AZA�A�A|�A{A�A��A��A�A�!A�#A��A��A33Az�A�hA��A�AVA1AG�A
�/A
-A�uAA�-AG�AZAx�A;dAZAG�A j@�S�@���@��`@�l�@��!@���@��/@�"�@�ff@��@���@��@��#@���@@���@�R@���@��@�Q�@��@�9X@�
=@�G�@�Ĝ@�  @��@�ƨ@�t�@�E�@�z�@�dZ@�@��@�9X@ۥ�@�S�@�"�@�ȴ@��@�p�@�%@���@��m@֧�@�@�X@ԓu@ӍP@�J@���@��@�  @ϝ�@�M�@�@�G�@�Ĝ@��
@�ȴ@�E�@��T@ə�@��
@�@���@���@Ł@Å@�V@���@�7L@���@�1@��w@�K�@��@�~�@�hs@�A�@�dZ@���@�^5@�bN@�1@��m@�C�@�-@��h@�&�@��`@��9@�  @���@��y@��@��-@�&�@��9@�1'@�l�@�J@�V@�(�@��@��!@��@�@��H@�ȴ@��+@�ff@�M�@�hs@��j@�Z@�1'@�1@���@�\)@��@�@���@��-@�Q�@��P@�"�@�C�@�l�@���@��w@�|�@�+@��!@�n�@�ff@�V@�E�@�?}@���@�Q�@��@�b@���@��;@���@���@�l�@�33@���@���@�ff@�@���@���@��7@�p�@��@��j@��@�I�@���@���@�|�@�+@��@�@�ȴ@�-@�$�@��@��@���@��9@��@�bN@�Q�@�1'@�  @��;@�ƨ@��@���@�S�@�K�@��@��@���@��@�X@�G�@�?}@�7L@���@��@�9X@�(�@���@��;@���@��F@���@�l�@�K�@�"�@���@���@�~�@�E�@�@�@�x�@�O�@�&�@���@���@�Ĝ@��j@��@��u@�j@���@��F@��P@�t�@�"�@���@�^5@�M�@�=q@�$�@�@��#@���@��7@�X@��@���@���@��j@���@��@�r�@�Q�@�b@��;@��w@�\)@�33@�+@�"�@��@�o@�@��@���@��T@��7@�hs@�&�@���@��9@���@�Q�@�1@�ƨ@��@�|�@��@�"�@�
=@��y@�ȴG�O�@�dZ@{��@r�1@lS�@d��@\�.@T�.@NkQ@J��@E�N@?/�@6($@/9�@)@#iD@�@�@�&@[�@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
�sB
�sB
�yB
�sB
�yB
�yB
�yB
�B
�B
�B
��B
��BBBB%BPB-Bl�B�BɺB��B�HB�B�B�B�B�B�B��B  B%B\B �B+B)�B)�B,B49B-B2-B?}BA�BG�BP�BdZB}�B�B�JB�hB�hB�uB�bB�DB�1B�Bu�BgmBbNB^5BYBR�BH�B>wB5?B$�B�BoBbBbBB�B�`B�ZB�ZB�NB��BĜB�B��B��B��B�DBy�Bq�BiyB\)BJ�B33B)�B'�B#�B�BVBB
��B
�B
�NB
�B
��B
��B
�\B
�B
t�B
jB
YB
P�B
G�B
<jB
2-B
+B
#�B
�B
�B
PB
B	��B	�B	�TB	�5B	�
B	��B	�}B	��B	�\B	� B	n�B	e`B	cTB	_;B	XB	H�B	=qB	33B	'�B	!�B	�B	VB	B��B��B�B�B�`B�BB�#B�B��B��B��B��BĜB��B�qB�^B�RB�?B�?B�?B�?B�9B�9B�9B�'B�B�B�B��B��B��B��B��B��B��B��B��B��B�uB�\B�JB�1B�%B�B�B}�B{�By�Bw�Bv�Bu�Bt�Bs�Br�Bq�Bo�Bn�Bm�Bl�Bk�Bk�BiyBhsBffBhsBgmBhsBffBe`BdZBcTBbNBcTBbNBcTBdZBdZBe`Be`Be`BffBiyBjBjBm�Bq�Bt�Bu�Bw�Bx�Bw�Bw�Bw�Bw�Bz�B|�B|�B}�B|�B}�B}�B}�B� B~�B� B� B�B�B�B�B�B�B�B�B�%B�+B�+B�7B�DB�JB�JB�PB�VB�bB�uB�{B�{B�{B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�-B�RB�XB�dB�qB�}B��B��BBÖBƨB��B��B��B��B��B��B�
B�)B�NB�TB�ZB�ZB�`B�sB�B�B�B��B��B��B��B	B	B	+B	1B	VB	hB	�B	�B	�B	 �B	$�B	%�B	%�B	,B	1'B	49B	6FB	7LB	9XB	<jB	>wB	>wB	@�B	D�B	C�B	G�B	K�B	M�B	O�B	P�B	R�B	T�B	W
B	XB	ZB	ZB	[#B	[#B	^5B	bNB	e`B	ffB	gmB	hsB	k�B	n�B	r�B	w�B	x�B	y�B	z�B	{�B	|�B	~�B	� B	� B	�B	�B	�B	�+B	�1B	�7B	�=B	�DB	�hB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�-B	�?B	�^B	�jB	�jB	�wB	�wB	�wB	�wB	��B	ĜB	ƨB	ƨB	ǮB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�#B	�)B	�)B	�)B	�)B	�/B	�;B	�BB	�HB	�NB	�TB	�fB	�mB	�mB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
+B
1B

=B
�B
�B
�B
&�B
+�B
3�B
=�B
CaB
KDB
P}B
VB
_;B
e�B
j�B
o�B
s�B
yXB
}<B
��B
�a111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111 B
��B
��B
��B
ؾB
��B
��B
��B
��B
��B
��B
�B
�0B
�OB
�ZB
�cB
�nB
��BNB\�B�LB��B�1B�uBڭBܹB��BݿBܹB��B�B�,B�PB��B�B+B&B(B/B$\B7B"RB/�B1�B7�BABT|BnBs,B|gB��B��B��B��B{cBxOBr,Be�BW�BRnBNWBI:BCB8�B.�B%mB	B�B�B �B �B�;BڸB՛BԓBԐB҈B�;B��B�SB�!B�B��B{�Bj%Ba�BY�BLxB;B#�BPBFB&BB
��B
�fB
�"B
��B
ҬB
�vB
��B
�HB
�B
s{B
e$B
Z�B
I�B
AVB
8$B
,�B
"�B
B
MB
B
B	��B	��B	�IB	�B	��B	θB	ǌB	�NB	�B	�tB	�B	p�B	_)B	U�B	S�B	O�B	H�B	9JB	.B	#�B	�B	gB	
:B��B�B�B�~B�SB�&B�
B��B��BȸBĤB��B��B�mB�MB�2B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�zB�dB�YB�SB�EB�BB�@B�.B�B}Bx�Bv�Bt�Bq�Bn�Bl�Bj�Bh�Bg�Bf�BexBduBcpBbkB`_B_VB^PB]LB\FB\FBZ=BY4BW(BY4BX2BY7BW,BV"BU BTBSBTBSBTBUBU!BV&BV'BV#BW,BZ@B[DB[GB^UBbtBe�Bf�Bh�Bi�Bh�Bh�Bh�Bh�Bk�Bm�Bm�Bn�Bm�Bn�Bn�Bn�Bp�Bo�Bp�Bp�Bq�Bs�Bs�Bt�Bt�Bt�Bt�Bt�Bv�Bw�Bw�By�B|B}B}B~BB�(B�6B�@B�?B�<B�SB�UB�_B�bB�lB�vB�|B�|B�pB��B��B��B��B��B��B�B�B�&B�5B�AB�EB�KB�PB�WB�eB��B��B��B��B��BĳB��B��B�	B�B�B�B�B�0B�@B�\B�pB�wB�B�B��B��B��B��B��B�B	#B	AB	
VB	XB	�B	�B	�B	�B	�B	!�B	$�B	&�B	(B	*B	-B	/,B	/,B	19B	5QB	4PB	8dB	<|B	>�B	@�B	A�B	C�B	E�B	G�B	H�B	J�B	J�B	K�B	K�B	N�B	S B	VB	WB	XB	Y'B	\7B	_JB	c^B	hB	i�B	j�B	k�B	l�B	m�B	o�B	p�B	p�B	q�B	s�B	u�B	w�B	x�B	y�B	z�B	{�B	�B	�#B	�-B	�9B	�DB	�_B	�iB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�"B	�"B	� B	� B	�.B	�HB	�QB	�PB	�VB	�[B	�^B	�bB	�hB	�mB	�vB	�xB	�vB	�xB	��B	B	ŤB	ƫB	ȵB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�	B	�B	�B	�B	�B	�B	�#B	�)B	�/B	�0B	�7B	�4B	�;B	�=B	�EB	�CB	�GB	�HB	�NB	�SB	�QB	�\B	�gB	�iB	�gB	�cB	�cB	�hB	�kB	�wB	�~B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��G�O�B	�SB
4B
_B
B
\B
$=B
.^B
4B
;�B
AB
F�B
O�B
V3B
[kB
`UB
d�B
i�B
m�B
qTB
t111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.09 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9996(+/-0.0001), vertically averaged dS =-0.015(+/-0.003) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Map scales: x=6,3; y=2,1. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                            201906040941452019060409414520190604094145  AO  ARCAADJP                                                                    20180413220052    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20180413220052  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20180413220052  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190604094145  IP                  G�O�G�O�G�O�                