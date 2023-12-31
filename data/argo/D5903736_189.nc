CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2017-07-21T00:00:52Z creation      
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
resolution        :�o     �  q�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    y�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �|   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �|   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �t   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �    HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �0   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �4   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �D   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �H   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �L   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20170721000052  20190604094028  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4051                            2C  D   APEX                            5368                            041511                          846 @���w�u1   @��}���@4��l�C��d�-1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B ffB��B  B  B   B(  B0  B8  B@  BH  BPffBX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ�C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt��Dy�3D�)D�L)D���D��\D�3D�V�D���D��HD��D�O\D��)D��D�HD�P D�yHD��HD��D�33D�{�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @z=q@��@��A�\A>�\A^�\A~�\A�G�A�G�A�G�A�G�A�G�A�G�A�G�B 
=B=qB��B��B��B'��B/��B7��B?��BG��BP
=BW��B_��Bg��Bo��Bw��B��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CZ�C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{D z=D �=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=D	z=D	�=D
z=D
�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=D z=D �=D!z=D!�=D"z=D"�=D#z=D#�=D$z=D$�=D%z=D%�=D&z=D&�=D'z=D'�=D(z=D(�=D)z=D)�=D*z=D*�=D+z=D+�=D,z=D,�=D-z=D-�=D.z=D.�=D/z=D/�=D0z=D0�=D1z=D1�=D2z=D2�=D3z=D3�=D4z=D4�=D5z=D5�=D6z=D6�=D7z=D7�=D8z=D8�=D9z=D9�=D:z=D:�=D;z=D;�=D<z=D<�=D=z=D=�=D>z=D>�=D?z=D?�=D@z=D@�=DAz=DA�=DBz=DB�=DCz=DC�=DDz=DD�=DEz=DE�=DFz=DF�=DGz=DG�=DHz=DH�=DIz=DI�=DJz=DJ�=DKz=DK�=DLz=DL�=DMz=DM�=DNz=DN�=DOz=DO�=DPz=DP�=DQz=DQ�=DRz=DR�=DSz=DS�=DTz=DT�=DUz=DU�=DVz=DV�=DWz=DW�=DXz=DX�=DYz=DY�=DZz=DZ�=D[z=D[�=D\z=D\�=D]z=D]�=D^z=D^�=D_z=D_�=D`z=D`�=Daz=Da�=Dbz=Db�=Dcz=Dc�=Ddz=Dd�=Dez=De�=Dfz=Df�=Dgz=Dg�=Dhz=Dh�=Diz=Di�=Djz=Dj�=Dkz=Dk�=Dlz=Dl�=Dmz=Dm�=Dnz=Dn�=Doz=Do�=Dpz=Dp�=Dqz=Dq�=Drz=Dr�=Dsz=Ds�=Dtz=Dt�
Dy�pD�	HD�IHD���D��{D�RD�S�D���D��gD��D�L{D��HD��3D�gD�MD�vgD�gD��D�0RD�x�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�JA��A��A���A�jA�"�A��A�VA�A��mA���Aە�A�7LA�S�A���A�?}A�VA���A�$�AΩ�A� �Aͧ�A��A˅A���Aʣ�A�E�A�;dA�M�A���AƃA�G�A�/Aç�A���A��PA�{A���A���A��A��A���A���A�33A��A�A�A��;A�"�A���A���A�(�A�p�A��HA�C�A��A�7LA���A�C�A��9A��A�+A�A�A�%A�v�A���A�oA��FA�(�A��A��/A��#A�9XA�ffA��
A�dZA�ZA���A�?}A��DA��A�~�A��-A�ZA�-A�VA��RA�ffA���A�A���A�r�A�
=A��A�K�A�A��A�`BA��uA�p�A���A��A�1A��A�
=A�bNA}��A{�A{33Az�Aw��Ar�An��Al�RAlAhM�Aet�Ad�`Ad�jAdjAb��A_��A]�A[XAY�AXZAW��AV�\AS�AQ�AOƨANJAM��AL��AL1'AJ�/AIXAH�AH��AH$�AFn�AD�\AB�DAAVA?dZA?�A>1'A<  A;\)A:�A9oA7A7�A5�;A41A21'A0ĜA/�A.M�A-ƨA-�^A-G�A+�wA)�
A(��A'��A$��A#�-A#`BA"ffA!��A!�A!?}A n�AXA�DA=qA�A�mAXAAt�A\)A
=A^5AdZA��AA��AA�mAS�A��A��A33AVA"�A�A|�AĜA�;A	�mA��A-Al�A=qAG�A�AQ�A�AA=qA/@�;d@��^@�r�@�^5@���@�A�@�~�@�%@�z�@�(�@��@�1@��@�G�@�  @�33@���@�5?@�w@�-@��@�/@��@�  @���@�%@��;@�S�@�ff@�bN@�~�@׍P@�E�@�%@��;@�@҇+@�p�@��m@θR@�x�@��@�(�@��@�"�@�v�@ȃ@�$�@��@ă@�I�@�9X@�bN@�r�@�z�@�j@�1'@þw@Ý�@�dZ@��@+@��@�`B@�&�@�%@��`@��@�Z@� �@��@�|�@��@�E�@���@��@�z�@� �@���@��\@���@�&�@�Z@��H@���@�X@�Q�@�1@�  @�;d@�J@�`B@���@���@���@�|�@�dZ@��y@���@���@��@��D@��@�r�@� �@�l�@�\)@�o@���@��H@�E�@���@��7@���@� �@��y@��-@���@� �@�ƨ@��@���@��@���@��@��@���@���@��@�S�@��H@�^5@��h@�p�@�&�@���@��9@�9X@�A�@�A�@�A�@�A�@�A�@�bN@�z�@�r�@�A�@��
@��P@�\)@��@���@��\@�~�@�@���@�@�G�@��@��`@���@�z�@� �@�1@�  @�t�@�E�@��#@�O�@�G�@�`B@�x�@�x�@�%@�Ĝ@��@��h@���@���@��@�`B@��j@�Z@�b@��m@���@���@�S�@�;d@�"�@�@���@��y@��@�ȴ@��R@��!@���@�~�@�ff@�^5@�ff@�^5@�$�@�{@��@���@�O�@��`@��u@�z�@�Q�@� �@���@��;@��F@�\)@�C�@�
=@�@��y@��H@���@�n�@�=q@��T@���@�G�@���@��9@��D@�I�@��@��@��
@��F@���@��@�\)@�K�@�;d@�33@�"�@��@��@�
=@��H@���@�v�@�M�@�5?@�-@�J@�@���@�G�@�j@�1'@��@�b@�  @��@�l�@��H@���@�M�@���@���@�V@��`@��9@���@��D@��u@��@�Z@��@�G�@/�@tK^@m��@g��@`��@X��@O��@G(@A2a@;Mj@6J@3�@-B�@(�@#e�@�o@�@-@��@
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A�JA��A��A���A�jA�"�A��A�VA�A��mA���Aە�A�7LA�S�A���A�?}A�VA���A�$�AΩ�A� �Aͧ�A��A˅A���Aʣ�A�E�A�;dA�M�A���AƃA�G�A�/Aç�A���A��PA�{A���A���A��A��A���A���A�33A��A�A�A��;A�"�A���A���A�(�A�p�A��HA�C�A��A�7LA���A�C�A��9A��A�+A�A�A�%A�v�A���A�oA��FA�(�A��A��/A��#A�9XA�ffA��
A�dZA�ZA���A�?}A��DA��A�~�A��-A�ZA�-A�VA��RA�ffA���A�A���A�r�A�
=A��A�K�A�A��A�`BA��uA�p�A���A��A�1A��A�
=A�bNA}��A{�A{33Az�Aw��Ar�An��Al�RAlAhM�Aet�Ad�`Ad�jAdjAb��A_��A]�A[XAY�AXZAW��AV�\AS�AQ�AOƨANJAM��AL��AL1'AJ�/AIXAH�AH��AH$�AFn�AD�\AB�DAAVA?dZA?�A>1'A<  A;\)A:�A9oA7A7�A5�;A41A21'A0ĜA/�A.M�A-ƨA-�^A-G�A+�wA)�
A(��A'��A$��A#�-A#`BA"ffA!��A!�A!?}A n�AXA�DA=qA�A�mAXAAt�A\)A
=A^5AdZA��AA��AA�mAS�A��A��A33AVA"�A�A|�AĜA�;A	�mA��A-Al�A=qAG�A�AQ�A�AA=qA/@�;d@��^@�r�@�^5@���@�A�@�~�@�%@�z�@�(�@��@�1@��@�G�@�  @�33@���@�5?@�w@�-@��@�/@��@�  @���@�%@��;@�S�@�ff@�bN@�~�@׍P@�E�@�%@��;@�@҇+@�p�@��m@θR@�x�@��@�(�@��@�"�@�v�@ȃ@�$�@��@ă@�I�@�9X@�bN@�r�@�z�@�j@�1'@þw@Ý�@�dZ@��@+@��@�`B@�&�@�%@��`@��@�Z@� �@��@�|�@��@�E�@���@��@�z�@� �@���@��\@���@�&�@�Z@��H@���@�X@�Q�@�1@�  @�;d@�J@�`B@���@���@���@�|�@�dZ@��y@���@���@��@��D@��@�r�@� �@�l�@�\)@�o@���@��H@�E�@���@��7@���@� �@��y@��-@���@� �@�ƨ@��@���@��@���@��@��@���@���@��@�S�@��H@�^5@��h@�p�@�&�@���@��9@�9X@�A�@�A�@�A�@�A�@�A�@�bN@�z�@�r�@�A�@��
@��P@�\)@��@���@��\@�~�@�@���@�@�G�@��@��`@���@�z�@� �@�1@�  @�t�@�E�@��#@�O�@�G�@�`B@�x�@�x�@�%@�Ĝ@��@��h@���@���@��@�`B@��j@�Z@�b@��m@���@���@�S�@�;d@�"�@�@���@��y@��@�ȴ@��R@��!@���@�~�@�ff@�^5@�ff@�^5@�$�@�{@��@���@�O�@��`@��u@�z�@�Q�@� �@���@��;@��F@�\)@�C�@�
=@�@��y@��H@���@�n�@�=q@��T@���@�G�@���@��9@��D@�I�@��@��@��
@��F@���@��@�\)@�K�@�;d@�33@�"�@��@��@�
=@��H@���@�v�@�M�@�5?@�-@�J@�@���@�G�@�j@�1'@��@�b@�  @��@�l�@��H@���@�M�@���@���@�V@��`@��9@���@��D@��u@��@�ZG�O�@�G�@/�@tK^@m��@g��@`��@X��@O��@G(@A2a@;Mj@6J@3�@-B�@(�@#e�@�o@�@-@��@
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB0!B0!B/B0!B5?B8RB9XB9XB:^BD�BXBr�B��B�;B��B
=B8RBK�Bn�B|�B�PB��B��B��B��B��B��B��B��B��B��B��B��B��B��B�'B�3B�3B�3B�3B�RB�jB��B�}B�qB�^B�FB�!B�B�B��B�B�B��B��B��B��B��B�oB�+B�Bx�Bl�Be`B[#BL�BG�BA�B49B$�B �B�B{BVB1B��B�B�yB�HB�B��BĜB��B� BffB\)BVBK�B:^B�B+B  B
��BB
��B
�ZB
��B
ƨB
�jB
��B
��B
�\B
z�B
gmB
\)B
J�B
=qB
8RB
33B
�B	��B	�B	�B	��B	�NB	��B	��B	��B	ƨB	�^B	�B	��B	�hB	�%B	�B	z�B	p�B	aHB	R�B	I�B	?}B	=qB	9XB	5?B	/B	'�B	&�B	#�B	 �B	�B	bB	+B	B��B��B�B�BB�)B�B��B��B��B��BĜB�}B�^B�LB�3B�-B�'B�B��B��B��B��B��B��B�{B�uB�oB�bB�\B�PB�JB�DB�DB�=B�7B�+B�+B�+B�=B�=B�7B�%B�B~�B{�By�Bx�Bw�Bu�Bs�Bq�Bo�Bl�BjBiyBgmBdZBbNB`BB_;B]/B^5B^5B]/B]/B]/B\)BYBXBW
BVBT�BR�BT�BS�BR�BS�BT�BXBXBZB]/B^5BaHBbNBcTBcTBffBgmBgmBgmBffBffBffBffBe`BdZBffBhsBgmBcTBaHB_;B^5B_;BaHBbNBe`BhsBk�Bn�Bm�Bm�Bl�Bm�Bo�Br�Bu�Bw�By�B� B�%B�7B�JB�VB�hB��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�?B�LB�FB�FB�LB�?B�FB�FB�dB�qBBŢBɺB��B��B�
B�B��B��B��B�B�#B�)B�)B�;B�mB�B�B�B��B��B��B	B	%B	
=B	PB	oB	�B	�B	�B	 �B	"�B	"�B	#�B	%�B	&�B	%�B	+B	1'B	33B	33B	6FB	7LB	<jB	@�B	A�B	@�B	?}B	>wB	@�B	D�B	H�B	L�B	P�B	ZB	[#B	\)B	\)B	]/B	`BB	cTB	cTB	ffB	k�B	r�B	v�B	x�B	{�B	}�B	~�B	� B	� B	�B	�=B	�VB	�bB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�?B	�LB	�RB	�XB	�dB	�}B	��B	ĜB	ŢB	ŢB	ƨB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�#B	�/B	�/B	�;B	�BB	�HB	�HB	�HB	�TB	�TB	�`B	�`B	�fB	�fB	�mB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
%B
%B
+B
	7B
	7B

=B

=B

=B

=B

=B
DB
JB
�B
IB
(�B
0�B
6B
;B
>�B
B�B
H�B
P�B
U�B
W�B
]B
cB
g�B
l=B
pB
uB
z^B
~�B
� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B&�B&�B%B&�B+�B.�B/�B/�B0�B;BNuBiB�'BՒB�NB �B.�BBBd�Bs;B��B��B�
B�;B�CB�GB�FB�,B�B� B�B�B�,B�6B�FB�rB�|B��B�|B�{B��B��B��B��B��B��B��B�kB�RB�RB�IB�VB�]B�IB�1B�+B�B��B��B}|BwWBo*Bb�B[�BQwBC(B>B7�B*�B<B BB
�B�B��B�NB��B��BשB�B�LB�B�,BviB\�BR�BLnBB7B0�BB
��B
�wB
�VB
��B
�nB
��B
�rB
�"B
��B
�cB
�1B
��B
qcB
]�B
R�B
AIB
3�B
.�B
)�B
EB	��B	�$B	�+B	�QB	��B	�qB	�ZB	�UB	�;B	��B	��B	�EB	� B	|�B	w�B	q{B	gAB	W�B	I�B	@XB	6B	4B	/�B	+�B	%�B	�B	�B	yB	fB	=B	B��B��B�}B�hB�<B��B��BϿBəBǌBƋB�jB�GB�'B�B��B��B��B��B��B��B��B�rB�[B�@B�4B�*B�%B�B�B�B�B��B��B��B��B�B}�B}�B}�B��B��B�B|�Bx�Bu�Br�Bp�Bo�Bn�BlxBjkBh^BfSBc@Ba5B`2B^#B[BYBV�BU�BS�BT�BT�BS�BS�BS�BR�BO�BN�BM�BL�BK�BI�BK�BJ�BI�BJ�BK�BN�BN�BP�BS�BT�BXBYBZBZB]B^'B^(B^&B] B]B]"B]"B\B[B] B_/B^)BZBXBU�BT�BU�BXBYB\B_2Bb@BeUBdLBdNBcFBdLBf[BilBl}Bn�Bp�Bv�B|�B�B�B�B�!B�<B�?B�HB�RB�cB�{B��B��B��B��B��B��B��B��B��B��B��B�B��B��B�B��B��B��B�B�)B�DB�YB�rBƖBʫB��B̺B˳B˵B˳B��B��B��B��B��B�$B�>B�PB�dB�uB�zB��B��B��B	 �B	B		B	8B	@B	KB	sB	�B	�B	�B	�B	�B	�B	!�B	'�B	)�B	)�B	,�B	. B	3B	76B	87B	7/B	6+B	5$B	74B	;LB	?eB	C}B	G�B	P�B	Q�B	R�B	R�B	S�B	V�B	ZB	ZB	]B	b3B	i[B	mvB	o�B	r�B	t�B	u�B	v�B	v�B	{�B	��B	�B	�
B	�CB	�QB	�cB	�qB	�pB	�uB	��B	�zB	��B	�B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�#B	�0B	�AB	�IB	�JB	�MB	�\B	�XB	�bB	�hB	�hB	�iB	�qB	�qB	�nB	�tB	�yB	�uB	�yB	�}B	�}B	ȔB	ʞB	ˠB	̭B	ͮB	ϾB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�%B	�0B	�CB	�IB	�MB	�PB	�VB	�_B	�_B	�fB	�hB	�pB	�vB	�wB	�yB	�~B	�uB	�wB	�xB	�zB	�~B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
 �B
 �B
 �B
�G�O�B
rB
�B
KB
'zB
,�B
1�B
54B
9~B
?!B
GRB
L�B
NaB
S�B
Y�B
^�B
b�B
f�B
k�B
p�B
u�B
w�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.09 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9998(+/-0), vertically averaged dS =-0.009(+/-0.001) in PSS-78.                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Map scales: x=6,3; y=2,1. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                            201906040940282019060409402820190604094028  AO  ARCAADJP                                                                    20170721000052    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20170721000052  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20170721000052  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190604094028  IP                  G�O�G�O�G�O�                