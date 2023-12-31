CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-18T14:14:05Z creation      
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200618141405  20220204114421  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               jA   AO  7662                            2C  D   APEX                            8312                            080318                          846 @��B^��1   @����g�@5�
=p���c65?|�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    jA   B   B   @���@�  A   A   A>ffA`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  BffB   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp��Bw��B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�33B�  B�  B���B�  B�  B�  B�  B�  C   C  C�C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN�CP�CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D��Dy�D  D� D  D� D	  D	� D
  D
� DfD� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D fD � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0�fD1  D1� D2  D2y�D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>y�D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DNfDN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt� Dy�D�HD�]D��qD��qD��D�aHD���D��\D�fD�T{D��qD��HD�)�D�a�DڮD���D�D�c�D�)D��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��R@��@��A�\A<��A^�\A~�\A�G�A�G�A�G�A�G�A�G�A�G�A�G�A�G�B��B��B
=B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bpp�Bw=qB��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B�B���B���B���B���B�B���B���B瞹B���B���B���B���B���B���C��C�C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CN�CP�CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��{C��{C�HC�HC��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C�HC��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C�HC��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C�HC��{C��{C��{C��{C��{C��{C��{C��{C�HC�HC��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{D z=D �=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D��Ds�D�=Dz=D�=Dz=D�=D	z=D	�=D
z=D �Dz=D�=Dz=D�=Dz=D�=D��D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D  �D z=D �=D!z=D!�=D"z=D"�=D#z=D#�=D$z=D$�=D%z=D%�=D&z=D&�=D'z=D'�=D(z=D(�=D)z=D)�=D*z=D*�=D+z=D+�=D,z=D,�=D-z=D-�=D.z=D.�=D/z=D/�=D0��D0�=D1z=D1�=D2s�D2�=D3z=D3�=D4z=D4�=D5z=D5�=D6z=D6�=D7z=D7�=D8z=D8�=D9z=D9�=D:z=D:�=D;z=D;�=D<z=D<�=D=z=D=�=D>s�D>�=D?z=D?�=D@z=D@�=DAz=DA�=DBz=DB�=DCz=DC�=DDz=DD�=DEz=DE�=DFz=DF�=DGz=DG�=DHz=DH�=DIz=DI�=DJz=DJ�=DKz=DK�=DLz=DL�=DMz=DN �DNz=DN�=DOz=DO�=DPz=DP�=DQz=DQ�=DRz=DR�=DSz=DS�=DTz=DT�=DUz=DU�=DVz=DV�=DWz=DW�=DXz=DX�=DYz=DY�=DZz=DZ�=D[z=D[�=D\z=D\�=D]z=D]�=D^z=D^�=D_z=D_�=D`z=D`�=Daz=Da�=Dbz=Db�=Dcz=Dc�=Ddz=Dd�=Dez=De�=Dfz=Df�=Dgz=Dg�=Dhz=Dh�=Diz=Di�=Djz=Dj�=Dkz=Dk�=Dlz=Dl�=Dmz=Dm�=Dnz=Dn�=Doz=Do�=Dpz=Dp�=Dqz=Dq�=Drz=Dr�=Dsz=Ds�=Dtz=Dt�=Dy�D�gD�Z>D���D�ڐD�D�^gD���D��{D��D�Q�D���D��gD�&�D�^�Dګ3D��D�3D�`�D�HD��>111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�r�A�hsA�jA��A��/A�ĜAڙ�A�r�A�O�A�E�A�7LA�$�A��A�bA�%A�VA�1A��A��A��mA��mA��mA��/A��/AپwA٬Aٗ�A�O�A׼jA��A��A���A̧�A�VA���A�|�A�1Aƛ�AŁA�z�A�(�AüjA�x�A�x�A�`BA¥�A��A��A��mA��;A���A���A�;dA�&�A��7A���A�VA�dZA���A��A��`A�ZA��A��HA��jA�A��yA��yA���A�ĜA�{A���A�VA��A�ffA�A�^5A�$�A�bA��9A��A�C�A�r�A���A�$�A���A�bA���A���A��!A�dZA�r�A��A���A���A�
=A�l�A�A� �A�ZA��mA��7A��A��A�7LA�5?A��yA�\)A�A��A�S�A���A���A���A���A��HA}/A{33Aw�PAtn�As/Ap�HAm��AkhsAh�DAfz�AdffAa�mA`ffA^�A]
=A\9XAZ�jAY%AU�AR�9AQXAPAO&�AMAM+ALr�AJ�AGp�AFbAE%AD�\AD{AB�A@Q�A?��A=�-A<ȴA;��A9�^A6$�A45?A3��A2�/A21'A1&�A.�A,��A+�A+�-A+/A)�A(9XA&jA#/A!S�A ��A A�#AS�A��AbNAXAbA�A�A�9A-A��A�7A��AM�A1A?}A�A+AAffAA�A��AbA  A�-A�A�7A~�A��A
�A
5?A	��A��AA�A�TA��A��A�A�Ap�A%A�A�PA`BA��A�A�-A�AS�@��
@���@���@�X@�bN@�n�@��h@��u@��
@��@�@�V@�9@�9X@�t�@@�M�@�5?@�@웦@�l�@�V@�h@��`@�r�@��@�33@�n�@���@���@�S�@�h@���@�n�@��/@���@���@��@���@�1'@ߥ�@�"�@���@�V@�O�@�(�@ڏ\@ڧ�@��@�-@�{@�E�@���@�X@ؼj@�bN@�r�@���@�ƨ@֧�@�E�@��@Չ7@�bN@�K�@Ұ!@��@�@��
@�5?@���@̴9@�A�@̓u@�Ĝ@�Z@��@�K�@�E�@�ff@��R@�%@���@�=q@�  @��;@�l�@���@��7@��9@���@���@�-@��@��-@���@�Ĝ@��@�ȴ@�^5@��F@��P@��9@���@��H@�1'@�A�@���@��y@��@�x�@��`@��j@���@�=q@��@�X@���@��@�z�@�`B@�+@�t�@�1'@��w@��@��@��@�K�@�n�@�bN@�t�@��/@���@���@�K�@�$�@�`B@�?}@�V@���@�I�@�(�@��@�"�@��@�ȴ@�M�@�E�@�J@��T@���@��-@�p�@�`B@�7L@��@�%@��@�r�@�Z@�I�@�9X@���@��@��
@��@�|�@�S�@�+@��@��@��@�o@��@�ȴ@���@�V@�=q@�@��h@�hs@�?}@��@���@��`@�Ĝ@��@�9X@���@��
@��@�S�@�33@�o@��y@���@���@���@���@�M�@��@�J@���@���@���@���@���@���@���@��h@�hs@�G�@��@���@��@���@��
@�t�@�l�@�33@���@�ȴ@��!@���@�ff@�-@�@���@���@�`B@�G�@�%@�Ĝ@��@�r�@�Z@�A�@� �@���@���@�t�@�S�@�+@�@��y@��@���@��!@�~�@�@��T@��-@�x�@�X@�7L@�%@���@��D@�bN@�1'@��F@�33@���@�ȴ@�v�@��@z_@v.�@p<�@f�@_��@W�&@QY�@K��@D�@>�h@8��@0,=@)��@#�m@�@@H@�@
��@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A�r�A�hsA�jA��A��/A�ĜAڙ�A�r�A�O�A�E�A�7LA�$�A��A�bA�%A�VA�1A��A��A��mA��mA��mA��/A��/AپwA٬Aٗ�A�O�A׼jA��A��A���A̧�A�VA���A�|�A�1Aƛ�AŁA�z�A�(�AüjA�x�A�x�A�`BA¥�A��A��A��mA��;A���A���A�;dA�&�A��7A���A�VA�dZA���A��A��`A�ZA��A��HA��jA�A��yA��yA���A�ĜA�{A���A�VA��A�ffA�A�^5A�$�A�bA��9A��A�C�A�r�A���A�$�A���A�bA���A���A��!A�dZA�r�A��A���A���A�
=A�l�A�A� �A�ZA��mA��7A��A��A�7LA�5?A��yA�\)A�A��A�S�A���A���A���A���A��HA}/A{33Aw�PAtn�As/Ap�HAm��AkhsAh�DAfz�AdffAa�mA`ffA^�A]
=A\9XAZ�jAY%AU�AR�9AQXAPAO&�AMAM+ALr�AJ�AGp�AFbAE%AD�\AD{AB�A@Q�A?��A=�-A<ȴA;��A9�^A6$�A45?A3��A2�/A21'A1&�A.�A,��A+�A+�-A+/A)�A(9XA&jA#/A!S�A ��A A�#AS�A��AbNAXAbA�A�A�9A-A��A�7A��AM�A1A?}A�A+AAffAA�A��AbA  A�-A�A�7A~�A��A
�A
5?A	��A��AA�A�TA��A��A�A�Ap�A%A�A�PA`BA��A�A�-A�AS�@��
@���@���@�X@�bN@�n�@��h@��u@��
@��@�@�V@�9@�9X@�t�@@�M�@�5?@�@웦@�l�@�V@�h@��`@�r�@��@�33@�n�@���@���@�S�@�h@���@�n�@��/@���@���@��@���@�1'@ߥ�@�"�@���@�V@�O�@�(�@ڏ\@ڧ�@��@�-@�{@�E�@���@�X@ؼj@�bN@�r�@���@�ƨ@֧�@�E�@��@Չ7@�bN@�K�@Ұ!@��@�@��
@�5?@���@̴9@�A�@̓u@�Ĝ@�Z@��@�K�@�E�@�ff@��R@�%@���@�=q@�  @��;@�l�@���@��7@��9@���@���@�-@��@��-@���@�Ĝ@��@�ȴ@�^5@��F@��P@��9@���@��H@�1'@�A�@���@��y@��@�x�@��`@��j@���@�=q@��@�X@���@��@�z�@�`B@�+@�t�@�1'@��w@��@��@��@�K�@�n�@�bN@�t�@��/@���@���@�K�@�$�@�`B@�?}@�V@���@�I�@�(�@��@�"�@��@�ȴ@�M�@�E�@�J@��T@���@��-@�p�@�`B@�7L@��@�%@��@�r�@�Z@�I�@�9X@���@��@��
@��@�|�@�S�@�+@��@��@��@�o@��@�ȴ@���@�V@�=q@�@��h@�hs@�?}@��@���@��`@�Ĝ@��@�9X@���@��
@��@�S�@�33@�o@��y@���@���@���@���@�M�@��@�J@���@���@���@���@���@���@���@��h@�hs@�G�@��@���@��@���@��
@�t�@�l�@�33@���@�ȴ@��!@���@�ff@�-@�@���@���@�`B@�G�@�%@�Ĝ@��@�r�@�Z@�A�@� �@���@���@�t�@�S�@�+@�@��y@��@���@��!@�~�@�@��T@��-@�x�@�X@�7L@�%@���@��D@�bN@�1'@��F@�33@���@�ȴG�O�@��@z_@v.�@p<�@f�@_��@W�&@QY�@K��@D�@>�h@8��@0,=@)��@#�m@�@@H@�@
��@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
B
B
B	��B	��B
  B
B
  B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
%B
%B
%B
	7B
DB
VB
bB
bB
PB	��B	��B
uB	��B
hB
k�B
�1B
�B
�B
�B
�B
�=B
�VB
�VB
�bB
��B
�FB
��B
��B
�B
�B	7B,BH�BZBhsB�B�{B��B�-B��B�B�/B��B%B\B�B'�B6FB<jB@�BJ�BJ�BL�BK�BT�BS�BT�BS�BVBZB[#BYBR�BM�BB�B9XB8RBJ�BA�BVBƨB�-B�uBz�BbNBbNBdZBYBO�B2-B(�B �B�B�B+B
�B
ɺB
�3B
��B
�B
m�B
^5B
[#B
W
B
N�B
(�B
�B	��B	�BB	�
B	ȴB	�FB	��B	�hB	�B	n�B	]/B	R�B	D�B	:^B	33B	(�B	�B	DB��B�B�B�mB�;B�B��B��B��B�FB�'B�B�B��B��B��B��B��B�uB�hB�bB�JB�DB�DB�7B�7B�+B�+B�%B�B�B�B�B�B�B|�B}�B|�B{�B|�B{�B{�B{�Bz�Bx�Bu�Bs�Br�Bo�Bl�Bl�Bk�BjBjBm�By�B�B�B�B�%B�B�B�B�%B�B�1B�B�B�B�B�B�B�B�B�B� B~�B|�B{�B{�Bz�Bx�Bx�By�Bx�Bw�Bv�Bu�Bx�Bv�Bz�Bx�By�B{�B{�B}�B� B�B�%B�1B�7B�7B�=B�=B�=B�=B�=B�JB�JB�PB�JB�PB�PB�JB�PB�PB�JB�PB�VB�PB�\B�hB�B�wB��BĜBȴB��B��B��B��B�
B�#B�#B�B�/B�;B�ZB�`B�B�yB�sB�fB�ZB�`B�fB�mB�TB�NB�HB�;B�;B�/B�)B�B��B��BȴB��B�BB�HB�`B�yB�B�/BƨB�FB�XBƨB��B�B��BĜBÖBB�}B�}B�wB�jBÖB��B��B��B��B�
B��B��BȴB�wB��B��B�/B�mB�)B�B	B��B��B��B	  B	B��B�B�TB�/B�#B�)B�fB�B��B	B	1B		7B	B	B	  B	B��B	JB	PB	%B	B	B	%B	DB	\B	bB	bB	bB	oB	�B	�B	�B	�B	"�B	&�B	)�B	,B	0!B	1'B	33B	7LB	9XB	<jB	?}B	C�B	H�B	L�B	N�B	N�B	O�B	Q�B	R�B	S�B	W
B	[#B	[#B	^5B	`BB	aHB	bNB	dZB	ffB	k�B	n�B	q�B	s�B	v�B	{�B	}�B	� B	�B	�B	�+B	�7B	�JB	�VB	�bB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�!B	�'B	�-B	�-B	�3B	�?B	�FB	�FB	�LB	�RB	�XB	�XB	�^B	�dB	�qB	��B	B	ĜB	ƨB	ƨB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�B	�B	�#B	�5B	�;B	�;B	�;B	�HB	�NB	�NB	�TB	�ZB	�B	��B
)B
\B
KB
(
B
.IB
3�B
9XB
A B
E�B
I�B
O�B
T�B
ZkB
_pB
c�B
jB
o�B
r�B
v�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�	B	�B	�	B	�B	�B	�	B	�B	�B	�B	�.B
;B
LB
XB
YB
GB	��B	��B
	mB	��B
aB
awB
~!B
w�B
v�B
z
B
z
B
�.B
�FB
�FB
�RB
��B
�4B
��B
��B
�B
�B
�B!�B>�BP B^UBv�B�ZB��B�
BĴB��B�	B�B��B2B�B�B,B2>B6WB@�B@�BB�BA�BJ�BI�BJ�BI�BK�BO�BP�BN�BH�BC�B8dB/.B.)B@�B7_B1B��B�B�ZBp�BX7BX7BZCBOBE�B(B�B�B�B�B
�B
�B
��B
�-B
��B
wB
c�B
T6B
Q%B
MB
D�B
�B
�B	��B	�OB	�B	��B	�WB	�B	�|B	y'B	d�B	SHB	IB	:�B	0zB	)PB	B	�B	dB�B��B�BݑB�`B�BB�$B��B��B�oB�PB�>B�,B� B�B��B��B��B��B��B��B�xB�rB�rBfBfB}ZB}[B|UB{OBzIBzIBzIBx=Bx=Bs Bt&Bs BrBs BrBrBrBqBoBk�Bi�Bh�Be�Bb�Bb�Ba�B`�B`�Bc�BpBw9Bx?BzLB|XBzLBzLBzLB|YByFB~eBzMByFB{SB{SBzNBzNByGBw;Bw;Bv5Bu0Bs$BrBrBqBoBoBpBoBnBm Bk�BoBm BqBoBpBrBrBt+Bv7B{VB|\B~hBnBnB�tB�tB�tB�tB�tB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�PB��B��B��B��B�B�B� B�B�<B�UB�UB�IB�aB�mBڌBےB�BߪBޤBܘBڌBےBܘBݟBنB؀B�zB�mB�mB�bB�\B�PB�,B�B��B�,B�uB�{BۓB߫B�B�cB��B�}B��B��B�B�9B�B��B��B��B��B��B��B��B��B��B�	B�B�B�@B�.B�B��B��B��B�
B�eBݢB�_B��B�?B�	B��B�B�3B�RB�"B��BيB�fB�ZB�`BܜB��B�"B�MB�dB�jB�SB�:B�4B�:B�(B	}B	�B�YB�SB�MB�YB	xB	�B	�B	�B	�B	�B	�B	�B	�B	�B	B	B	 .B	":B	&RB	'XB	)dB	-}B	/�B	2�B	5�B	9�B	>�B	B�B	EB	EB	FB	HB	I!B	J'B	M9B	QQB	QQB	TcB	VpB	WvB	X|B	Z�B	\�B	a�B	d�B	g�B	i�B	l�B	rB	t B	v,B	x8B	{JB	}VB	bB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�%B	�1B	�7B	�7B	�DB	�IB	�IB	�OB	�UB	�UB	�[B	�gB	�nB	�nB	�tB	�zB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�$B	�$B	�0B	�=B	�=B	�=B	�=B	�CB	�CB	�IB	�[B	�aB	�aB	�aB	�nB	�tB	�tB	�zG�O�B	�CB	�B
LB
B
nB
,B
$kB
)�B
/yB
7AB
;�B
@B
F B
KB
P�B
U�B
Y�B
`6B
e�B
h�B
l�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.09 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9997(+/-0.0001), vertically averaged dS =-0.01(+/-0.004) in PSS-78.                                                                                                                                                                                        Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144212022020411442120220204114421  AO  ARCAADJP                                                                    20200618141405    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200618141405  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200618141405  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114421  IP                  G�O�G�O�G�O�                