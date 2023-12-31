CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-19T17:08:57Z creation      
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200619170857  20220204114412  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  7662                            2C  D   APEX                            8312                            080318                          846 @؊Ky	�1   @؊�H^@7�O�;dZ�cЃn��1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   B   B   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A���A�  A�  B ffB  B��B  B   B(  B/��B8  B@  BH  BP  BX��B^ffBg��Bp  Bx  B�  B�  B�  B�  B�33B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C�fC  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C3�fC6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D0��D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7fD7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DX��DYy�DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt��Dy��D�${D�c�D���D���D�RD�i�D��{D��=D�
D�VfD���D��\D�&fD�]�Dښ�D�߮D��D�FD��D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��R@��@��A�\A>�\A^�\A~�\A�G�A�G�A�G�A�G�A�{A�G�A�G�B 
=B��B=qB��B��B'��B/=qB7��B?��BG��BO��BXp�B^
=Bg=qBo��Bw��B��B���B���B���B�B���B���B���B���B���B���B���B���B���B���B���B�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C��C��C��C��C	��C��C��C��C��C��C�\C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3�\C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C�HC��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C�HC��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{D z=D �=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=D	z=D	�=D
z=D
�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D �D��D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=D z=D �=D!z=D!�=D"z=D"�=D#z=D#�=D$z=D$�=D%z=D%�=D&z=D&�=D'z=D'�=D(z=D(�=D)z=D)�=D*z=D*�=D+z=D+�=D,z=D,�=D-z=D-�=D.z=D.�=D/z=D/�=D0z=D0��D1z=D1�=D2z=D2�=D3z=D3�=D4z=D4�=D5z=D5�=D6z=D7 �D7z=D7�=D8z=D8�=D9z=D9�=D:z=D:�=D;z=D;�=D<z=D<�=D=z=D=�=D>z=D>�=D?z=D?�=D@z=D@�=DAz=DA�=DBz=DB�=DCz=DC�=DDz=DD�=DEz=DE�=DFz=DF�=DGz=DG�=DHz=DH�=DIz=DI�=DJz=DJ�=DKz=DK�=DLz=DL�=DMz=DM�=DNz=DN�=DOz=DO�=DPz=DP�=DQz=DQ�=DRz=DR�=DSz=DS�=DTz=DT�=DUz=DU�=DVz=DV�=DWz=DW�=DXz=DX��DYs�DY�=DZz=DZ�=D[z=D[�=D\z=D\�=D]z=D]�=D^z=D^�=D_z=D_�=D`z=D`�=Daz=Da�=Dbz=Db�=Dcz=Dc�=Ddz=Dd�=Dez=De�=Dfz=Df�=Dgz=Dg�=Dhz=Dh�=Diz=Di�=Djz=Dj�=Dkz=Dk�=Dlz=Dl�=Dmz=Dm�=Dnz=Dn�=Doz=Do�=Dpz=Dp�=Dqz=Dq�=Drz=Dr�=Dsz=Ds�=Dtz=Dt�
Dy��D�!�D�`�D��D��D�qD�gD���D��\D�)D�S�D���D��{D�#�D�Z�Dژ D���D� D�C3D�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��#A��A��#A��;A��TA��/A��;A��/A��;A��A��A���A���A���A���A���A���A���A���A��A��A��A��A��`A�ȴA�^5A�n�A�1A��TA�\)A�33A��Aɏ\AȲ-Aǟ�A�r�AŮA��FA��A�&�A�C�A��`A�/A��A��A�5?A�(�A���A���A�~�A��FA�&�A��jA��A��A��A��A�/A��mA��wA���A�33A���A�t�A��#A��#A�ffA�oA��A���A�A�bNA�x�A��A��A���A�{A�p�A��A�A�A��RA�ZA��A���A�5?A�33A��+A��A�O�A��TA�oA���A��HA�~�A���A��wA��A�x�A��A���A�G�A�x�A���A��A�bNA�7LA��DAoA|9XA{+A{%Ay�-Aw�7Arz�An=qAk
=Ah��Ah=qAgl�Ac�FA`A�A^jA]�hA\�A[��AZ$�AX�/AW�
AU�ASƨAQ��AOANn�AL��ALI�AJ=qAF=qAC�#ABE�A@bA=�^A<�9A<(�A;t�A:��A:E�A9�hA8�A7��A6�`A6{A5hsA4z�A3�7A2�jA2v�A1|�A0��A0bA/G�A.�!A.(�A,�A+%A)��A(��A'��A'C�A&v�A%�A%�mA%��A%S�A${A#oA"�A"jA!�Ap�A-A�!A�A~�A�A��A��A1'A�A�A��A�A�hAXAA~�Ax�Al�A~�AZA�A��A�hAdZA�A��A
�yA	�A	��A	G�A	
=A�A�AXAQ�A�TAx�A
=A��A�^AXA�uA��AG�A �yA r�@�K�@�O�@�Z@��@�33@�^5@���@��@�dZ@�ȴ@�hs@��@��@�7@�bN@�dZ@�7@�9X@띲@�|�@�t�@�dZ@�@�`B@�t�@�
=@�R@�M�@�J@�-@�u@�~�@��T@�X@�r�@���@�
=@�E�@�Z@�l�@�ȴ@ٲ-@���@���@Ձ@��/@�b@�+@ҏ\@�-@�p�@�1@Η�@�?}@̼j@��@�C�@��@�r�@Ƨ�@��@ċD@�Z@�  @���@��-@��`@��F@�J@���@�O�@���@��u@� �@�t�@�@�v�@�E�@���@�bN@�I�@�j@�t�@�C�@���@�E�@�G�@�Ĝ@�ƨ@�K�@��@���@��R@��^@�r�@��y@���@��@�9X@���@���@��@��@�~�@�v�@���@��@���@�+@�?}@�Ĝ@��D@�bN@��@���@�
=@�E�@�@��-@�?}@��@��@�;d@�+@���@��@�ȴ@��H@���@���@�^5@���@���@���@�r�@�z�@���@� �@�dZ@��@���@��+@�n�@�ff@�^5@�=q@�5?@���@���@�x�@�x�@�X@�X@�p�@���@���@�Ĝ@��j@�r�@� �@�  @���@��P@�"�@�
=@��y@�~�@�$�@��#@��^@��h@��@���@�z�@�I�@�1'@��@��
@��@��@�\)@�
=@���@��\@�n�@�5?@�{@�@��T@���@�@�@��-@��h@�p�@�G�@�&�@�%@��/@��9@��@��D@�bN@��@���@�;d@��@���@��!@���@���@�n�@�~�@�ff@�~�@�M�@���@��@���@�x�@�7L@�V@���@�A�@�1@���@�K�@��!@�n�@�v�@�~�@�v�@�n�@�-@��T@�@��@�`B@�G�@���@��`@���@��j@��u@�Z@�A�@�1'@���@���@�dZ@�S�@�+@�@��R@��\@��+@��+@�^5@�V@�M�@�5?@��T@���@�@�_@���@w>�@nYK@ga@[n/@R�@K8@D*�@<l"@6Q@0�K@,�@'�@"-@�.@4@J�@Y@e�@o111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A��#A��A��#A��;A��TA��/A��;A��/A��;A��A��A���A���A���A���A���A���A���A���A��A��A��A��A��`A�ȴA�^5A�n�A�1A��TA�\)A�33A��Aɏ\AȲ-Aǟ�A�r�AŮA��FA��A�&�A�C�A��`A�/A��A��A�5?A�(�A���A���A�~�A��FA�&�A��jA��A��A��A��A�/A��mA��wA���A�33A���A�t�A��#A��#A�ffA�oA��A���A�A�bNA�x�A��A��A���A�{A�p�A��A�A�A��RA�ZA��A���A�5?A�33A��+A��A�O�A��TA�oA���A��HA�~�A���A��wA��A�x�A��A���A�G�A�x�A���A��A�bNA�7LA��DAoA|9XA{+A{%Ay�-Aw�7Arz�An=qAk
=Ah��Ah=qAgl�Ac�FA`A�A^jA]�hA\�A[��AZ$�AX�/AW�
AU�ASƨAQ��AOANn�AL��ALI�AJ=qAF=qAC�#ABE�A@bA=�^A<�9A<(�A;t�A:��A:E�A9�hA8�A7��A6�`A6{A5hsA4z�A3�7A2�jA2v�A1|�A0��A0bA/G�A.�!A.(�A,�A+%A)��A(��A'��A'C�A&v�A%�A%�mA%��A%S�A${A#oA"�A"jA!�Ap�A-A�!A�A~�A�A��A��A1'A�A�A��A�A�hAXAA~�Ax�Al�A~�AZA�A��A�hAdZA�A��A
�yA	�A	��A	G�A	
=A�A�AXAQ�A�TAx�A
=A��A�^AXA�uA��AG�A �yA r�@�K�@�O�@�Z@��@�33@�^5@���@��@�dZ@�ȴ@�hs@��@��@�7@�bN@�dZ@�7@�9X@띲@�|�@�t�@�dZ@�@�`B@�t�@�
=@�R@�M�@�J@�-@�u@�~�@��T@�X@�r�@���@�
=@�E�@�Z@�l�@�ȴ@ٲ-@���@���@Ձ@��/@�b@�+@ҏ\@�-@�p�@�1@Η�@�?}@̼j@��@�C�@��@�r�@Ƨ�@��@ċD@�Z@�  @���@��-@��`@��F@�J@���@�O�@���@��u@� �@�t�@�@�v�@�E�@���@�bN@�I�@�j@�t�@�C�@���@�E�@�G�@�Ĝ@�ƨ@�K�@��@���@��R@��^@�r�@��y@���@��@�9X@���@���@��@��@�~�@�v�@���@��@���@�+@�?}@�Ĝ@��D@�bN@��@���@�
=@�E�@�@��-@�?}@��@��@�;d@�+@���@��@�ȴ@��H@���@���@�^5@���@���@���@�r�@�z�@���@� �@�dZ@��@���@��+@�n�@�ff@�^5@�=q@�5?@���@���@�x�@�x�@�X@�X@�p�@���@���@�Ĝ@��j@�r�@� �@�  @���@��P@�"�@�
=@��y@�~�@�$�@��#@��^@��h@��@���@�z�@�I�@�1'@��@��
@��@��@�\)@�
=@���@��\@�n�@�5?@�{@�@��T@���@�@�@��-@��h@�p�@�G�@�&�@�%@��/@��9@��@��D@�bN@��@���@�;d@��@���@��!@���@���@�n�@�~�@�ff@�~�@�M�@���@��@���@�x�@�7L@�V@���@�A�@�1@���@�K�@��!@�n�@�v�@�~�@�v�@�n�@�-@��T@�@��@�`B@�G�@���@��`@���@��j@��u@�Z@�A�@�1'@���@���@�dZ@�S�@�+@�@��R@��\@��+@��+@�^5@�V@�M�@�5?@��T@���G�O�@�_@���@w>�@nYK@ga@[n/@R�@K8@D*�@<l"@6Q@0�K@,�@'�@"-@�.@4@J�@Y@e�@o111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�fB)�B6FBI�BW
BbNBffBn�Bu�Bu�Bz�B�B~�B�1B�%B�+B�7B�=B�bB��B��B��B��B��B��B��B��B��B�uB�oB�PB�DB�7B�+B� B~�By�Bx�Bw�Bq�Bl�BjBaHBN�BE�B<jB1'B$�B�BuBJBB��B��B�B��BŢB�wB�dB�B��B��B�PB~�Bt�BZBH�B:^BPBDB
��B
�B
�B
�B
ĜB
�?B
��B
��B
�hB
�B
XB
)�B
bB
%B
B	��B	�yB	ŢB	��B	�DB	x�B	r�B	l�B	W
B	@�B	/B	&�B	�B	�B	JB	B��B�B�ZB�
BɺBŢBÖB��B�RB��B�\B�PB�=B�B� B� B~�B|�B{�Bz�By�Bw�Bu�Bs�Bq�Bo�Bn�Bk�Bk�BjBjBjBk�Bl�BjBm�BgmBgmBe`BdZBcTBbNB`BB_;B_;B^5B_;B[#B]/B]/B]/BVBT�BQ�BP�BN�BN�BL�BL�BK�BK�BK�BL�BN�BM�BP�BQ�BO�BM�BJ�BF�BE�BE�BC�BB�BA�B@�B@�B?}B;dB;dB:^B:^B:^B8RB9XB8RB7LB7LB6FB6FB7LB6FB7LB9XB7LB8RB7LB9XB<jB<jB;dB<jB;dB<jB;dB9XB8RB9XB8RB8RB8RB8RB8RB9XB9XB9XB9XB9XB8RB8RB<jB<jB<jB<jB=qB=qB=qB?}BA�B@�BA�BB�BB�BC�BG�BI�BI�BI�BI�BI�BI�BI�BJ�BL�BS�BZBZB[#B^5BaHB`BBaHBaHBaHB`BBcTBe`BjBjBjBl�Bq�Br�Br�Br�Bt�Bt�Bu�Bv�Bw�Bw�By�Bz�B|�B� B�B}�B}�B�B�%B�+B�DB�PB�bB�\B�{B�{B�{B�uB�{B��B�oB�{B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�!B�-B�-B�FB�jB�}B��BBƨB��B��B��B��B��B��B��B��B�
B�)B�HB�ZB�sB�yB�B�B�B�B��B��B��B	B	%B	+B		7B	PB	\B	�B	�B	�B	�B	�B	�B	�B	!�B	#�B	&�B	(�B	)�B	)�B	,B	33B	5?B	9XB	;dB	<jB	?}B	C�B	E�B	F�B	H�B	L�B	O�B	VB	YB	[#B	^5B	`BB	aHB	cTB	dZB	gmB	iyB	k�B	k�B	l�B	m�B	p�B	q�B	r�B	r�B	s�B	u�B	v�B	w�B	y�B	z�B	{�B	}�B	�B	�B	�1B	�=B	�PB	�VB	�hB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�3B	�3B	�9B	�?B	�FB	�RB	�^B	�jB	�qB	��B	��B	��B	B	ÖB	ĜB	ŢB	ƨB	ȴB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�)B	�)B	�)B	�B	�-B
�B
�B
2B
�B
)�B
4TB
<�B
C�B
I�B
N�B
Q�B
VSB
[�B
^�B
eFB
k�B
nIB
r-B
t111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�'B݈B!B-dB@�BN&BYjB]�Be�Bl�Bl�Bq�Bz-BvBLB}AB~GB�SB�YB�~B��B��B��B��B��B��B��B��B��B��B��B�nB�bB�UB~JBwBvBp�Bo�Bn�Bh�Bc�Ba�BXjBE�B<�B3�B(MBB�B
�BsB�IB�$B��B�=B��B��B��B��B�3B��B��B��Bv.Bk�BQTB?�B1�B�B�B
�,B
��B
�B
�QB
��B
��B
�5B
��B
��B
zZB
O[B
!KB
�B	�vB	�kB	�9B	��B	��B	�*B	��B	p2B	jB	c�B	NiB	7�B	&~B	LB	"B	�B	�B�lB�HB�B��B�tB�%B�B�B��B��B�:B��B��B��B{�BwrBwrBvlBt`BsYBrTBqNBoBBm6Bk)BiBgBfBb�Bb�Ba�Ba�Ba�Bb�Bd Ba�BeB^�B^�B\�B[�BZ�BY�BW�BV�BV�BU�BV�BR�BT�BT�BT�BM|BLvBIeBH^BFRBFRBDFBDGBCABCABCABDGBFSBEMBH_BIfBGYBEMBB<B>#B=B=B;B:
B9B7�B7�B6�B2�B2�B1�B1�B1�B/�B0�B/�B.�B.�B-�B-�B.�B-�B.�B0�B.�B/�B.�B0�B3�B3�B2�B3�B2�B3�B2�B0�B/�B0�B/�B/�B/�B/�B/�B0�B0�B0�B0�B0�B/�B/�B3�B3�B3�B3�B4�B4�B4�B6�B9B8B9B:B:B;B?-BA9BA9BA9BA9BA9BA:BA:BBABDMBKwBQ�BQ�BR�BU�BX�BW�BX�BX�BX�BW�BZ�B\�Ba�Ba�Ba�Bd
Bi(Bj.Bj.Bj.Bl:Bl:BmABnGBoMBoMBqYBr_BtlBw~Bx�BurBurBz�B}�B~�B��B��B��B��B��B��B��B��B��B�B��B��B�
B�B�#B�#B�)B�/B�)B�5B�;B�fB�~B��B��B��B��B��B��B��B��B��B��B�B�
B�#B�BB�HB�NB�TB�ZB�`B�gB�rB΄BӣB��B��B��B��B��B�
B�B�/B�5B�AB�lB��B��B��B	 �B	�B	�B	�B		B	"B	(B	(B	(B	4B	AB	MB	^B	 kB	!qB	!qB	#}B	*�B	,�B	0�B	2�B	3�B	6�B	;
B	=B	>B	@'B	D@B	GRB	MwB	P�B	R�B	U�B	W�B	X�B	Z�B	[�B	^�B	`�B	b�B	b�B	c�B	eB	hB	iB	j!B	j!B	k'B	m4B	n:B	o@B	qLB	rQB	sWB	udB	xvB	z�B	�B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�&B	�9B	�EB	�^B	�dB	�jB	�vB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	� B	� B	� B	�&B	�-B	�3B	�9B	�?B	�EB	�?B	�?B	�EB	�EB	�KB	�KB	�QB	�QB	�cB	�oB	�uB	ЂB	шB	шB	шB	шB	ӔB	ӔG�O�B	�B	�B	�[B
�B
�B
[B
!dB
+�B
4 B
;KB
A<B
FB
ISB
M�B
R�B
V9B
\�B
cB
e�B
i�B
k�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.09 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9998(+/-0.0001), vertically averaged dS =-0.008(+/-0.002) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144122022020411441220220204114412  AO  ARCAADJP                                                                    20200619170857    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200619170857  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200619170857  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114412  IP                  G�O�G�O�G�O�                