CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-18T14:14:03Z creation      
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200618141403  20220204114420  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               bA   AO  7662                            2C  D   APEX                            8312                            080318                          846 @�؜�3AX1   @�؝>���@6��$�/�c~�t�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    bA   B   B   @�  @�  A   A   A@  A`  A�  A�  A���A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0ffB8ffB?��BH  BQ��BW33B`  Bg��Bp  Bx  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CI�fCK�fCN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz�C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D%��D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D/��D0� D1  D1� D2  D2� D3  D3� D4  D4� D4��D5y�D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt� Dy��D�,)D�X�D��D��=D�!HD�W�D���D��D�"=D�Z=D���Dǃ�D�RD�U�Dڔ)D��)D�!HD�Y�D�D��q111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��A�\A>�\A^�\A~�\A�G�A�{A�G�A�G�A�G�A�G�A�G�A�G�B��B��B��B��B'��B0
=B8
=B?=qBG��BQ=qBV�
B_��Bg=qBo��Bw��B��B���B���B�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���Bߞ�B���B���B���B���B���B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI�\CK�\CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cz�C{��C}��C��C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{D z=D �=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=D	z=D	�=D
z=D
�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D �Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=D z=D �=D!z=D!�=D"z=D"�=D#z=D#�=D$z=D$�=D%z=D%��D&z=D&�=D'z=D'�=D(z=D(�=D)z=D)�=D*z=D*�=D+z=D+�=D,z=D,�=D-z=D-�=D.z=D.�=D/z=D/��D0z=D0�=D1z=D1�=D2z=D2�=D3z=D3�=D4z=D4��D5s�D5�=D6z=D6�=D7z=D7�=D8z=D8�=D9z=D9�=D:z=D:�=D;z=D;�=D<z=D<�=D=z=D=�=D>z=D>�=D?z=D?�=D@z=D@�=DAz=DA�=DBz=DB�=DCz=DC�=DDz=DD�=DEz=DE�=DFz=DF�=DGz=DG�=DHz=DH�=DIz=DI�=DJz=DJ�=DKz=DK�=DLz=DL�=DMz=DM�=DNz=DN�=DOz=DO�=DPz=DP�=DQz=DQ�=DRz=DR�=DSz=DS�=DTz=DT�=DUz=DU�=DVz=DV�=DWz=DW�=DXz=DX�=DYz=DY�=DZz=DZ�=D[z=D[�=D\z=D\�=D]z=D]�=D^z=D^�=D_z=D_�=D`z=D`�=Daz=Da�=Dbz=Db�=Dcz=Dc�=Ddz=Dd�=Dez=De�=Dfz=Df�=Dgz=Dg�=Dhz=Dh�=Diz=Di�=Djz=Dj�=Dkz=Dk�=Dlz=Dl�=Dmz=Dm�=Dnz=Dn�=Doz=Do�=Dpz=Dp�=Dqz=Dq�=Drz=Dr�=Dsz=Ds�=Dtz=Dt�=Dy�D�)HD�U�D��>D��\D�gD�T�D���D�޹D�\D�W\D���Dǀ�D�qD�R�DڑHD��HD�gD�V�D��D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�dZA�-A��mA�ffA�ZA�VA�?}A��A��yA��`A��/A۝�AۋDA�|�A�|�AۋDA۩�Aۥ�Aۧ�A۬Aە�A��;Aϲ-A�&�AϑhA�1'A�n�AȶFA���A���A��7A�r�A�z�A��hA�"�A��-A�XA�XA�  A��A�7LA���A�K�A���A�$�A�^5A��mA���A�7LA�t�A�A��/A�|�A�VA�VA�=qA�v�A�(�A���A�XA�K�A��9A���A��PA�S�A�M�A�-A�bA��A��wA�t�A�/A��7A��yA�(�A��7A�7LA���A���A�$�A���A�Q�A�hsA�9XA��wA��A�-A���A�A��mA�hsA�A�ȴA�ffA�?}A�G�A�7LA���A��;A�M�A~v�A|n�Azz�Axz�Au�Au�AsC�Arz�Ap�uAohsAn�yAm�7Al1'Aj �Ai�Ah�Af��Ad$�Aa�mA`v�A_��A_`BA^I�A\�DA[VAY�;AX��AW�mAW\)AV�AV�AUO�AS�TAR�AP�HAOVAL��AJ��AJJAIXAG�7AF�AE��AC�AB��AA�;A@jA?A=7LA:VA8^5A7��A7XA6-A5\)A3&�A2��A2  A0��A/�-A/C�A.��A.z�A.1A,�uA+�A*�RA*~�A)+A&�`A%`BA#�#A#��A"I�A!�hA ȴA�FA/A��A�AoAt�A�A��AM�A
=A{A�!A��A�#An�AS�A=qAG�A
bNA	��A	hsA	S�A	"�A	A�yA�\A$�A�7A9XA��AG�A��A��A��A�^A+A ��@���@�o@�t�@�5?@��@��@��`@��@�X@�;d@��@�@�w@���@���@�I�@�b@�K�@⟾@�=q@��@�z�@ߥ�@ޟ�@݉7@ܣ�@�M�@��
@�dZ@�"�@��@�E�@��T@���@�(�@��;@ӥ�@ӍP@҇+@�7L@���@��@�p�@�/@д9@���@�dZ@�~�@̬@��@��@�33@�1'@��@��@§�@��y@�M�@�r�@�
=@��R@���@�E�@���@�Ĝ@�K�@��@���@�G�@� �@�;d@���@�{@��@���@���@�@�G�@�r�@��@��F@�S�@��#@�&�@���@��u@�j@�S�@��R@���@���@��+@���@�/@�1'@�+@�~�@��@���@�?}@�%@�z�@��
@�ff@��#@��T@��h@�O�@���@��@�33@���@�$�@�?}@��u@�(�@��
@��@�C�@�o@��@���@�{@�/@���@�9X@��@��@��w@�
=@��R@��H@���@�-@�5?@�=q@�E�@�-@���@��^@�@��T@�@�J@���@��T@���@��-@�X@��j@��u@��D@�z�@��@��@�bN@�(�@���@��@�ƨ@���@���@�dZ@��y@�E�@���@��T@���@���@�7L@�Ĝ@���@�r�@�(�@�  @��F@�\)@�C�@�;d@�+@�"�@�o@��@���@�ȴ@���@��\@�$�@���@�V@�n�@��@��h@���@���@���@��-@��@�%@��`@��9@��@��@�j@�Z@�1'@��@��@���@�S�@��@��@���@�V@�-@�@���@�p�@��@�%@���@���@��/@��9@���@�j@���@��P@�C�@�"�@�@���@���@���@���@���@��\@��+@�=q@���@�hs@�X@�G�@�/@��@���@���@��D@�r�@�Q�@�9X@�(�@� �@�b@�  @���@��m@�ƨ@��@�\)@�dZ@�dZ@�C�@�
=@��@���@���@���@�v�@�n�@�~�@��+@��+@�$�@���@���@�_@x��@k�a@d�j@\�@R�@Nu@G�
@A��@:�2@5J�@/��@*Ov@&��@!|@�@}�@x@��@��@�)111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A�dZA�-A��mA�ffA�ZA�VA�?}A��A��yA��`A��/A۝�AۋDA�|�A�|�AۋDA۩�Aۥ�Aۧ�A۬Aە�A��;Aϲ-A�&�AϑhA�1'A�n�AȶFA���A���A��7A�r�A�z�A��hA�"�A��-A�XA�XA�  A��A�7LA���A�K�A���A�$�A�^5A��mA���A�7LA�t�A�A��/A�|�A�VA�VA�=qA�v�A�(�A���A�XA�K�A��9A���A��PA�S�A�M�A�-A�bA��A��wA�t�A�/A��7A��yA�(�A��7A�7LA���A���A�$�A���A�Q�A�hsA�9XA��wA��A�-A���A�A��mA�hsA�A�ȴA�ffA�?}A�G�A�7LA���A��;A�M�A~v�A|n�Azz�Axz�Au�Au�AsC�Arz�Ap�uAohsAn�yAm�7Al1'Aj �Ai�Ah�Af��Ad$�Aa�mA`v�A_��A_`BA^I�A\�DA[VAY�;AX��AW�mAW\)AV�AV�AUO�AS�TAR�AP�HAOVAL��AJ��AJJAIXAG�7AF�AE��AC�AB��AA�;A@jA?A=7LA:VA8^5A7��A7XA6-A5\)A3&�A2��A2  A0��A/�-A/C�A.��A.z�A.1A,�uA+�A*�RA*~�A)+A&�`A%`BA#�#A#��A"I�A!�hA ȴA�FA/A��A�AoAt�A�A��AM�A
=A{A�!A��A�#An�AS�A=qAG�A
bNA	��A	hsA	S�A	"�A	A�yA�\A$�A�7A9XA��AG�A��A��A��A�^A+A ��@���@�o@�t�@�5?@��@��@��`@��@�X@�;d@��@�@�w@���@���@�I�@�b@�K�@⟾@�=q@��@�z�@ߥ�@ޟ�@݉7@ܣ�@�M�@��
@�dZ@�"�@��@�E�@��T@���@�(�@��;@ӥ�@ӍP@҇+@�7L@���@��@�p�@�/@д9@���@�dZ@�~�@̬@��@��@�33@�1'@��@��@§�@��y@�M�@�r�@�
=@��R@���@�E�@���@�Ĝ@�K�@��@���@�G�@� �@�;d@���@�{@��@���@���@�@�G�@�r�@��@��F@�S�@��#@�&�@���@��u@�j@�S�@��R@���@���@��+@���@�/@�1'@�+@�~�@��@���@�?}@�%@�z�@��
@�ff@��#@��T@��h@�O�@���@��@�33@���@�$�@�?}@��u@�(�@��
@��@�C�@�o@��@���@�{@�/@���@�9X@��@��@��w@�
=@��R@��H@���@�-@�5?@�=q@�E�@�-@���@��^@�@��T@�@�J@���@��T@���@��-@�X@��j@��u@��D@�z�@��@��@�bN@�(�@���@��@�ƨ@���@���@�dZ@��y@�E�@���@��T@���@���@�7L@�Ĝ@���@�r�@�(�@�  @��F@�\)@�C�@�;d@�+@�"�@�o@��@���@�ȴ@���@��\@�$�@���@�V@�n�@��@��h@���@���@���@��-@��@�%@��`@��9@��@��@�j@�Z@�1'@��@��@���@�S�@��@��@���@�V@�-@�@���@�p�@��@�%@���@���@��/@��9@���@�j@���@��P@�C�@�"�@�@���@���@���@���@���@��\@��+@�=q@���@�hs@�X@�G�@�/@��@���@���@��D@�r�@�Q�@�9X@�(�@� �@�b@�  @���@��m@�ƨ@��@�\)@�dZ@�dZ@�C�@�
=@��@���@���@���@�v�@�n�@�~�@��+@��+@�$�@���G�O�@�_@x��@k�a@d�j@\�@R�@Nu@G�
@A��@:�2@5J�@/��@*Ov@&��@!|@�@}�@x@��@��@�)111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBBBB1BJBVB\B\BVBbBuBbBoB�B�B�B&�B'�B(�B,B(�B�B
k�B
k�BoBM�B��B�B��B�B6FBL�B��B|�Bv�BhsB��B�B�'B�3B��B�B�)B�#B�#B�
B�ZB�B�B�B�B��B�B�B��B�B�B%�BP�Br�Be`BS�B�B�B�B��BVB(�B-B49B<jB>wB8RB2-B-B�BJB�B�BǮB�XB��B�VBy�Bp�BcTBVBH�B<jB)�BPB
��B
�yB
��B
�RB
��B
��B
x�B
l�B
aHB
R�B
@�B
1'B
!�B
\B
%B	��B	��B	�B	�;B	�B	��B	ƨB	�XB	�B	��B	��B	�\B	~�B	v�B	o�B	m�B	hsB	\)B	Q�B	J�B	E�B	A�B	>wB	;dB	5?B	1'B	)�B	�B	�B	PB	B��B�B�B�sB�NB�;B�B��B��BɺB��B�dB�3B��B��B��B��B��B��B��B��B��B�uB�uB�uB�oB�bB�=B�7B�1B�+B�+B�B|�Bp�Bn�Bk�Bk�Bl�BffBcTBbNBaHB[#BZBS�BQ�BR�BQ�BL�BG�BD�BD�BB�B=qB;dB7LB6FB:^B<jB>wB?}B?}B?}B@�B=qB:^B=qB?}B?}B?}B@�B@�B;dB:^B9XB8RB2-B(�B&�B �B$�B%�B"�B&�B.B-B-B,B,B,B,B,B,B,B+B+B+B)�B)�B)�B+B,B'�B%�B%�B%�B%�B$�B$�B$�B$�B&�B+B0!B6FB;dB@�BE�BH�BL�BP�BR�BT�BZB\)B[#BZBYBVBYB]/Be`BgmBhsBk�Bl�Bm�Bp�Bq�Br�Bw�By�By�Bz�B{�B}�B~�B� B�B�B�B�B�B�B�B�B�B�=B�PB�VB�VB�\B��B��B��B��B��B��B��B��B�B�3B�FB�RB�^B�dB�}BÖB��B��B��B��B��B�B�B�B�)B�ZB�mB�yB�B�B�B�B��B��B��B��B	B	
=B	VB	\B	hB	{B	�B	"�B	%�B	'�B	+B	+B	,B	,B	-B	/B	0!B	1'B	2-B	33B	49B	7LB	9XB	:^B	;dB	?}B	@�B	A�B	C�B	G�B	N�B	S�B	XB	ZB	^5B	`BB	ffB	iyB	m�B	o�B	p�B	t�B	v�B	w�B	x�B	z�B	~�B	�B	�B	�B	�1B	�7B	�DB	�PB	�VB	�VB	�\B	�bB	�hB	�oB	�uB	��B	��B	��B	��B	��B	�B	�B	�B	�B	�'B	�-B	�3B	�?B	�RB	�^B	�XB	�^B	�^B	�dB	�jB	�jB	�jB	�qB	�wB	�}B	��B	B	ÖB	ĜB	ƨB	ƨB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�#B	�)B	�/B	�5B	�5B	�;B	�BB	�BB	�NB	�ZB	�`B	�`B	�fB	�fB	�mB	�sB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B
?B
�B
�B
]B
(�B
/5B
7�B
;B
A�B
FtB
OBB
S&B
YB
^�B
dtB
iB
nB
oOB
s�B
w�B
|�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B
��B
��B
��B
�BB)B/B/B)B5B
HB5B	BBSBlB�B�B�B�B"�B�BeB
beB
beB	CBD�B��B��B�BdB-	BC�B�Bs�Bm�B_FB�{B��B��B�B�PB��B��B��B��B��B�&B�oB�oB�{B�{B�B�pB�]B�BcB{B�BG�BisB\%BJ�BvB�lB�xB�B"B�B#�B+B33B5@B/B(�B#�B}BB�yB��B��B�,B�vB�-Bp�Bg~BZ/BL�B?�B3HB �B1B
��B
�]B
ſB
�:B
��B
�xB
o�B
cyB
X7B
I�B
7uB
(B
�B
RB	�B	��B	�B	�xB	�5B	�B	��B	��B	�UB	�B	��B	��B	�]B	u�B	m�B	f�B	d�B	_wB	S.B	H�B	A�B	<�B	8�B	5B	2lB	,GB	(0B	!B	�B	�B	\B�&B��B��B�B߂B�^B�KB�(B�	B��B��B��B�xB�GB�B��B��B��B��B��B��B��B��B��B��B��B��B�zB�VB�PBJB~DB~EBy&Bt	Bg�Be�Bb�Bb�Bc�B]�BZqBYkBXfBRABQ<BKBIBJBIBC�B>�B;�B;�B9�B4�B2�B.oB-iB1�B3�B5�B6�B6�B6�B7�B4�B1�B4�B6�B6�B6�B7�B7�B2�B1�B0|B/vB)RB BB�BB
B�BB%:B$5B$5B#/B#/B#/B#/B#/B#/B#/B")B")B")B!#B!$B!$B"*B#0BBBBBBBBBBB"*B'IB-nB2�B7�B<�B?�BC�BHBJBL$BQCBSNBRIBQCBP=BM+BP=BTUB\�B^�B_�Bb�Bc�Bd�Bg�Bh�Bi�Bn�Bq Bq BrBsBuBvBw%Bx+Bx+Bx+Bx+Bx+Bx+By1By1B{>B�aB�tB�zB�zB��B��B��B��B��B��B��B��B�B�1B�UB�hB�tB��B��B��B��B��B��B� B�B�B�$B�7B�=B�IB�zBތB��B�B�B��B��B��B��B��B�B�=B	ZB	sB	yB	�B	�B	�B	�B	�B	B	"B	"B	##B	##B	$)B	&6B	'<B	(BB	)HB	*NB	+TB	.fB	0rB	1xB	2~B	6�B	7�B	8�B	:�B	>�B	E�B	KB	O(B	Q5B	UMB	WZB	]}B	`�B	d�B	f�B	g�B	k�B	m�B	n�B	o�B	q�B	vB	z(B	z(B	|5B	FB	�LB	�YB	�eB	�kB	�kB	�qB	�wB	�}B	��B	��B	��B	��B	��B	��B	��B	�B	�'B	�.B	�.B	�:B	�@B	�FB	�RB	�dB	�pB	�jB	�pB	�pB	�vB	�|B	�|B	�|B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�(B	�.B	�3B	�9B	�?B	�EB	�EB	�KB	�RB	�RB	�^B	�jB	�pB	�pB	�vB	�vB	�}B	߃B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��G�O�B	�MB
�B
	�B
kB
�B
&BB
.�B
2"B
8�B
=�B
FNB
J2B
P"B
U�B
[B
`B
eB
fZB
j�B
n�B
s�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.09 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9998(+/-0.0001), vertically averaged dS =-0.009(+/-0.005) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144202022020411442020220204114420  AO  ARCAADJP                                                                    20200618141403    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200618141403  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200618141403  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114420  IP                  G�O�G�O�G�O�                