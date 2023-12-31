CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-03-03T18:02:33Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         (Matthew Alkire, University of Washington      @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
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
resolution        :�o     �  qt   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {h   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �\   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �\   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �P   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �    HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �$   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �(   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �,Argo profile    3.1 1.2 19500101000000  20180303180233  20190604094144  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4051                            2C  D   APEX                            5368                            041511                          846 @�P�韾�1   @�P�����@4����F�d~��n�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @���@���A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR�CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dts3Dy)D�)D�)HD���D��qD��D�L{D�\D�^fD��D�H�D�r�D��\D�=D�P�D�m�D���D�=D�K�D�x�D��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��R@��@��A�\A>�\A^�\A~�\A�G�A�G�A�G�A�G�A�G�A�G�A�G�A�G�B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CR�CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{D z=D �=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=D	z=D	�=D
z=D
�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=D z=D �=D!z=D!�=D"z=D"�=D#z=D#�=D$z=D$�=D%z=D%�=D&z=D&�=D'z=D'�=D(z=D(�=D)z=D)�=D*z=D*�=D+z=D+�=D,z=D,�=D-z=D-�=D.z=D.�=D/z=D/�=D0z=D0�=D1z=D1�=D2z=D2�=D3z=D3�=D4z=D4�=D5z=D5�=D6z=D6�=D7z=D7�=D8z=D8�=D9z=D9�=D:z=D:�=D;z=D;�=D<z=D<�=D=z=D=�=D>z=D>�=D?z=D?�=D@z=D@�=DAz=DA�=DBz=DB�=DCz=DC�=DDz=DD�=DEz=DE�=DFz=DF�=DGz=DG�=DHz=DH�=DIz=DI�=DJz=DJ�=DKz=DK�=DLz=DL�=DMz=DM�=DNz=DN�=DOz=DO�=DPz=DP�=DQz=DQ�=DRz=DR�=DSz=DS�=DTz=DT�=DUz=DU�=DVz=DV�=DWz=DW�=DXz=DX�=DYz=DY�=DZz=DZ�=D[z=D[�=D\z=D\�=D]z=D]�=D^z=D^�=D_z=D_�=D`z=D`�=Daz=Da�=Dbz=Db�=Dcz=Dc�=Ddz=Dd�=Dez=De�=Dfz=Df�=Dgz=Dg�=Dhz=Dh�=Diz=Di�=Djz=Dj�=Dkz=Dk�=Dlz=Dl�=Dmz=Dm�=Dnz=Dn�=Doz=Do�=Dpz=Dp�=Dqz=Dq�=Drz=Dr�=Dsz=Ds�=DtmpDyfD�HD�&gD���D�D��>D�I�D�|{D�[�D��D�E�D�o�D��{D��\D�M�D�j�D���D�\D�H�D�vD��>11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A���A���A���A���A���A���A�  A�A�{A�M�A���A�A�A�=qA�/A�"�A�"�A�A��mA�ZA��A��A��HA���A��A��A��HA��+A��PA��\A��hA��DA��A�~�A�^5A���A�9XA���A���A��FA�r�A��HA���A�n�A�;dA��A��
A���A�v�A��A�|�A�oA��-A�ffA���A�9XA�+A�"�A�jA���A�oA�|�A��A�O�A�ĜA���A�=qA�O�A��#A���A�Q�A��mA��A���A��A���A��RA��`A��
A�1A��`A��9A��mA�t�A���A��jA���A�1A��uA�bA��TA�A�G�A�t�A�?}A�1A��
A��A���A�9XA��RA��A�1A��DA�1A�
=A�Q�A��A�-A�7LA|9XAv��As�^Ap�DAn1Al1'Aj��AhVAfE�Ac�Ab�DA`��A^��A[�AX��AW��AV�uAT��AS�-AR��AR1AQ;dAQVAO�AMdZAK/AH�AFA�AD�RAC�^AA+A=dZA=/A<�HA:��A9dZA7��A61'A41'A2��A2I�A2JA1�;A1�wA0��A/�#A.��A.A-��A-t�A,�/A,9XA*�!A)��A)\)A)%A(ZA'`BA&(�A&(�A&JA$��A#��A#&�A!&�A   A?}AA�A�^A��A�A�mA`BA��A��A�A�7A�/AA`BA"�AȴAbA%A�AbNAQ�AJA
�DA	ƨA��AƨA��AI�A��A�FA�hAVA��AffAO�@�=q@���@�(�@�33@�p�@�J@�|�@�33@��@�z�@��;@@���@�\@�r�@��;@�@�\@�O�@��
@�$�@� �@�t�@�+@ް!@��@�A�@ە�@��@���@�o@�^5@�-@�@���@Լj@�ƨ@ҟ�@Гu@�S�@�=q@�G�@̓u@˶F@��@�hs@ț�@�r�@� �@�"�@�@őh@ēu@�S�@�{@���@��w@��!@���@�I�@���@�o@��@��^@���@��@�X@��@�@�@���@���@��7@�7L@��9@��@��F@���@�"�@��@�?}@�V@�1'@�b@��m@���@��;@�S�@�;d@�@���@�{@�@��7@�G�@�/@��@��j@�1'@�ƨ@�\)@�@��+@�M�@���@���@��@���@���@�b@�l�@��@�V@�J@��@���@�?}@��u@�Q�@�Q�@�r�@�r�@��@��@�1@��
@��w@�o@�~�@�@��7@�X@��@�Q�@��@��
@��F@��P@�t�@�+@��H@���@�~�@�V@�-@��@�{@��#@�x�@�?}@�/@��@�%@���@��D@��@���@��@�|�@�\)@�C�@���@���@���@��+@�^5@���@��@�`B@�O�@�X@�7L@���@�I�@�(�@��@�  @��m@���@���@�dZ@�
=@��@��@�ȴ@��R@���@�ff@��@���@�p�@�?}@�/@��u@�bN@�I�@�1@���@�ƨ@�|�@�K�@�33@�@��R@�~�@�{@���@���@�p�@�&�@���@��`@��@�I�@�1'@�1@��w@�\)@�"�@�~�@�=q@�-@���@�X@�?}@�&�@���@�r�@�9X@��
@���@�S�@�+@���@�n�@�-@�{@�@��7@��7@��@�`B@��@���@�Ĝ@���@��@�r�@�bN@�I�@�9X@�(�@��m@��w@�t�@�+@��y@���@�E�@�$�@���@���@��h@�`B@�?}@��@��@���@���@��@�9X@���@�t�@�l�@�dZ@�\)@�S�@�]�@|4n@r��@jp;@c
=@]N<@Tr�@K@D�U@>�!@8�@1�^@+�6@&5?@!�-@z@<6@�@\)@�@�	11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A���A���A���A���A���A���A���A�  A�A�{A�M�A���A�A�A�=qA�/A�"�A�"�A�A��mA�ZA��A��A��HA���A��A��A��HA��+A��PA��\A��hA��DA��A�~�A�^5A���A�9XA���A���A��FA�r�A��HA���A�n�A�;dA��A��
A���A�v�A��A�|�A�oA��-A�ffA���A�9XA�+A�"�A�jA���A�oA�|�A��A�O�A�ĜA���A�=qA�O�A��#A���A�Q�A��mA��A���A��A���A��RA��`A��
A�1A��`A��9A��mA�t�A���A��jA���A�1A��uA�bA��TA�A�G�A�t�A�?}A�1A��
A��A���A�9XA��RA��A�1A��DA�1A�
=A�Q�A��A�-A�7LA|9XAv��As�^Ap�DAn1Al1'Aj��AhVAfE�Ac�Ab�DA`��A^��A[�AX��AW��AV�uAT��AS�-AR��AR1AQ;dAQVAO�AMdZAK/AH�AFA�AD�RAC�^AA+A=dZA=/A<�HA:��A9dZA7��A61'A41'A2��A2I�A2JA1�;A1�wA0��A/�#A.��A.A-��A-t�A,�/A,9XA*�!A)��A)\)A)%A(ZA'`BA&(�A&(�A&JA$��A#��A#&�A!&�A   A?}AA�A�^A��A�A�mA`BA��A��A�A�7A�/AA`BA"�AȴAbA%A�AbNAQ�AJA
�DA	ƨA��AƨA��AI�A��A�FA�hAVA��AffAO�@�=q@���@�(�@�33@�p�@�J@�|�@�33@��@�z�@��;@@���@�\@�r�@��;@�@�\@�O�@��
@�$�@� �@�t�@�+@ް!@��@�A�@ە�@��@���@�o@�^5@�-@�@���@Լj@�ƨ@ҟ�@Гu@�S�@�=q@�G�@̓u@˶F@��@�hs@ț�@�r�@� �@�"�@�@őh@ēu@�S�@�{@���@��w@��!@���@�I�@���@�o@��@��^@���@��@�X@��@�@�@���@���@��7@�7L@��9@��@��F@���@�"�@��@�?}@�V@�1'@�b@��m@���@��;@�S�@�;d@�@���@�{@�@��7@�G�@�/@��@��j@�1'@�ƨ@�\)@�@��+@�M�@���@���@��@���@���@�b@�l�@��@�V@�J@��@���@�?}@��u@�Q�@�Q�@�r�@�r�@��@��@�1@��
@��w@�o@�~�@�@��7@�X@��@�Q�@��@��
@��F@��P@�t�@�+@��H@���@�~�@�V@�-@��@�{@��#@�x�@�?}@�/@��@�%@���@��D@��@���@��@�|�@�\)@�C�@���@���@���@��+@�^5@���@��@�`B@�O�@�X@�7L@���@�I�@�(�@��@�  @��m@���@���@�dZ@�
=@��@��@�ȴ@��R@���@�ff@��@���@�p�@�?}@�/@��u@�bN@�I�@�1@���@�ƨ@�|�@�K�@�33@�@��R@�~�@�{@���@���@�p�@�&�@���@��`@��@�I�@�1'@�1@��w@�\)@�"�@�~�@�=q@�-@���@�X@�?}@�&�@���@�r�@�9X@��
@���@�S�@�+@���@�n�@�-@�{@�@��7@��7@��@�`B@��@���@�Ĝ@���@��@�r�@�bN@�I�@�9X@�(�@��m@��w@�t�@�+@��y@���@�E�@�$�@���@���@��h@�`B@�?}@��@��@���@���@��@�9X@���@�t�@�l�@�dZ@�\)G�O�@�]�@|4n@r��@jp;@c
=@]N<@Tr�@K@D�U@>�!@8�@1�^@+�6@&5?@!�-@z@<6@�@\)@�@�	11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB0!B0!B0!B0!B/B/B/B0!B1'B7LBN�B�jB��B��B��B��B��B�B�B+BhB�B)�B;dBF�BP�BiyB|�B�JB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�{B�oB�hB�\B�JB�=B�7B�+B�B�B}�By�Bu�Br�Bn�Bl�BjBffBcTB[#BT�BL�BG�BE�BB�B>wB6FB+B%�B �B{BB��B�B�yB�`B�B��B��B��B��B��B�B�VBq�BdZBP�BG�BF�BD�BA�B<jB33B�B%B
�B
�#B
ŢB
�qB
�!B
��B
��B
�oB
hsB
<jB
�B
B	�B	�B	��B	��B	�-B	��B	�oB	�=B	|�B	m�B	[#B	M�B	F�B	?}B	49B	.B	'�B	$�B	 �B	�B	�B	
=B��B��B�B�mB�HB�5B�)B�#B�B��B��BǮBŢB��B�wB�qB�jB�jB�dB�XB�LB�?B�9B�3B�-B�'B�B�B��B��B��B��B��B��B��B��B��B��B�uB�\B�JB�7B�%B�B�B~�B}�B{�Bz�Bw�Bu�Bq�Bo�Bn�Bn�Bm�Bl�Bk�BjBjBjBiyBgmBffBe`BdZBcTBe`BgmBgmBffBffBe`BcTBaHB^5B^5B]/B]/B]/B]/B\)B]/B]/B]/B]/B]/B]/B]/B]/B^5B^5B^5B^5B_;BaHBcTBffBgmBgmBgmBjBk�Bk�Bm�Bn�Bq�Bs�Bs�Bt�Bs�Bv�Bw�By�B}�B~�B�B�B�B�B�+B�7B�PB�VB�VB�hB�{B�uB��B��B��B��B��B��B�B�-B�?B�LB�}B��BĜBƨBǮB��B��B�B�
B�
B�
B�B�B�/B�5B�5B�;B�BB�NB�`B�B�B��B��B��B��B	B	B	%B	DB	PB	VB	bB	bB	bB	uB	�B	�B	�B	�B	$�B	&�B	+B	-B	/B	33B	49B	8RB	<jB	>wB	C�B	F�B	F�B	G�B	I�B	N�B	Q�B	R�B	S�B	VB	VB	VB	XB	[#B	]/B	_;B	dZB	hsB	k�B	k�B	o�B	r�B	t�B	u�B	v�B	w�B	x�B	|�B	~�B	~�B	�B	�B	�B	�%B	�%B	�1B	�=B	�=B	�DB	�JB	�PB	�\B	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�'B	�-B	�3B	�3B	�3B	�9B	�?B	�LB	�RB	�^B	�jB	�}B	�}B	��B	��B	ÖB	ĜB	ƨB	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�#B	�#B	�/B	�5B	�;B	�HB	�TB	�ZB	�fB	�mB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
%B
%B
%B
+B
1B
1B
1B
	7B
	7B
	7B
DB
DB
JB
VB
\B
\B
VB
VB
\B
�B
7B
"�B
,�B
2-B
6�B
>(B
GzB
M�B
S�B
Z7B
_�B
c�B
iyB
mCB
p;B
utB
z^B
~�B
�AB
�%11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B!QB!LB!QB!NB CB IB FB!QB"TB(vB@B��B��B��B��B��B��B��B��B�?B}B�BB,sB7�BA�BZ�Bm�B}QB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�tB�mB�eB}OB{IBz=Bx4Bv+BsBoBj�Bf�Bc�B_�B]�B[�BWyBTfBL5BFB=�B8�B6�B3�B/�B'^BB�B�B�B�'B��BݮBڞBցB�?B�B��B��B��B��B�:B�Bb�BU�BBB8�B7�B5�B2�B-�B$oB�B
�gB
��B
�mB
��B
��B
�nB
�/B
�B
��B
Y�B
-�B
�B	�jB	��B	˅B	�4B	��B	��B	�IB	��B	{�B	ndB	_B	L�B	?MB	8$B	0�B	%�B	�B	uB	]B	IB	:B	
B��B��B�WB� B��B��B��BͷB̮BʤB�|B�XB�<B�2B�B�
B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�vB�`B�WB�OB�DB�3B�&B�B��B}�Bz�Bw�Bu�Br�Bp�Bo�Bm�Bl�BimBgcBcJBa>B`9B`8B_1B^,B](B\$B\#B\$B[BYBXBWBU�BT�BWBYBYBXBXBWBT�BR�BO�BO�BN�BN�BN�BN�BM�BN�BN�BN�BN�BN�BN�BN�BN�BO�BO�BO�BO�BP�BR�BU BXBYBYBYB\*B]1B]1B_;B`?BcSBe^Be_BfeBe`BhsBiwBk�Bo�Bp�Br�Bs�Bs�Bt�Bx�Bz�B~�B�B�B�B� B�B�'B�:B�@B�bB��B��B��B��B��B��B�!B�,B�BB�JB�QB�cBĕBǧBȰBȬBȫBʹB��B��B��B��B��B��B��B��B�B�TB�ZB�oB�B�B��B��B��B��B��B��B	�B	�B	�B	B		'B	<B	MB	ZB	tB	�B	�B	�B	 �B	$�B	%�B	)�B	.B	0B	5/B	8AB	8@B	9CB	;RB	@pB	C�B	D�B	E�B	G�B	G�B	G�B	I�B	L�B	N�B	P�B	U�B	ZB	]B	]B	a2B	dDB	fPB	gVB	h\B	i`B	jlB	n�B	p�B	p�B	s�B	t�B	v�B	w�B	w�B	y�B	{�B	{�B	|�B	}�B	~�B	��B	��B	�B	�B	�B	�&B	�+B	�1B	�=B	�@B	�HB	�OB	�PB	�ZB	�eB	�kB	�qB	�yB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�	B	�B	�B	�$B	�(B	�4B	�QB	�XB	�]B	�cB	�sB	āB	ƊB	ȓB	əB	ʞB	ˣB	˨B	̮B	̫B	κB	ϾB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�"B	�2B	�9B	�@B	�AB	�JB	�QB	�VB	�XB	�\B	�cB	�cB	�aB	�_B	�oB	�jB	�oB	�xB	�yB	�xB	�~B	�wB	��B	��B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B	��B	��G�O�B
iB
�B
 B
B
#�B
(�B
/�B
8�B
?<B
EB
K�B
Q	B
UvB
Z�B
^�B
a�B
f�B
k�B
p(B
s�B
w�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.09 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9996(+/-0.0001), vertically averaged dS =-0.014(+/-0.002) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Map scales: x=6,3; y=2,1. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                            201906040941442019060409414420190604094144  AO  ARCAADJP                                                                    20180303180233    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20180303180233  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20180303180233  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190604094144  IP                  G�O�G�O�G�O�                