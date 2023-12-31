CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-02-21T18:02:38Z creation      
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
_FillValue                    �,Argo profile    3.1 1.2 19500101000000  20180221180238  20190604094144  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4051                            2C  D   APEX                            5368                            041511                          846 @�N�^1   @�N���@4������d~ȴ9X1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D �fD!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt` DynD�
D�<�D�xRD���D��D�E�D���D�ٚD�fD�7�D�vDǯ\D�RD�:�Dړ�D��qD�D�3�D�uqD��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��R@��@��A�\A>�\A^�\A~�\A�G�A�G�A�G�A�G�A�G�A�G�A�G�A�G�B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{D z=D �=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=D	z=D	�=D
z=D
�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=D ��D �=D!z=D!�=D"z=D"�=D#z=D#�=D$z=D$�=D%z=D%�=D&z=D&�=D'z=D'�=D(z=D(�=D)z=D)�=D*z=D*�=D+z=D+�=D,z=D,�=D-z=D-�=D.z=D.�=D/z=D/�=D0z=D0�=D1z=D1�=D2z=D2�=D3z=D3�=D4z=D4�=D5z=D5�=D6z=D6�=D7z=D7�=D8z=D8�=D9z=D9�=D:z=D:�=D;z=D;�=D<z=D<�=D=z=D=�=D>z=D>�=D?z=D?�=D@z=D@�=DAz=DA�=DBz=DB�=DCz=DC�=DDz=DD�=DEz=DE�=DFz=DF�=DGz=DG�=DHz=DH�=DIz=DI�=DJz=DJ�=DKz=DK�=DLz=DL�=DMz=DM�=DNz=DN�=DOz=DO�=DPz=DP�=DQz=DQ�=DRz=DR�=DSz=DS�=DTz=DT�=DUz=DU�=DVz=DV�=DWz=DW�=DXz=DX�=DYz=DY�=DZz=DZ�=D[z=D[�=D\z=D\�=D]z=D]�=D^z=D^�=D_z=D_�=D`z=D`�=Daz=Da�=Dbz=Db�=Dcz=Dc�=Ddz=Dd�=Dez=De�=Dfz=Df�=Dgz=Dg�=Dhz=Dh�=Diz=Di�=Djz=Dj�=Dkz=Dk�=Dlz=Dl�=Dmz=Dm�=Dnz=Dn�=Doz=Do�=Dpz=Dp�=Dqz=Dq�=Drz=Dr�=Dsz=Ds�=DtZ=DyhQD�)D�9�D�uqD�� D��D�B�D���D�ֹD��D�4�D�s3DǬ{D�qD�7�Dڐ�D�D�
>D�0�D�r�D��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��hA���A���A���A���A��A���A���A���A��uA���A���A���A���A�z�A�jA�ffA�ffA�hsA�ffA�ffA�ffA�`BA�`BA�bNA�dZA�ffA�l�A�n�A�jA�ffA�hsA�^5A�I�A�{A���A���A�
=A�\)A���A�oA���A�p�A���A��DA��A�
=A�?}A�ZA��A��jA��`A�  A���A��A��
A�G�A��TA���A�A�A�A�9XA��HA��+A�hsA��A���A��-A�XA���A��wA�dZA��DA���A�{A�O�A��A���A�`BA��A�+A��A���A�E�A�\)A�$�A��TA�n�A��A���A�M�A��A��A��;A��A��A�dZA�+A��
A���A�"�A�A��A�
=A��hA�  A�z�A�(�A���A���A��A}XA|~�A{?}At=qAq�
An��AmK�Al�AhȴAghsAe�Ae+Ad�!AcƨAbn�Aa�7Aa33A`A_�TA_XA]�AZI�AW��AWK�AV�!AU\)AR�RAP��AO��AN�AM�AMK�AKl�AH�9AG�-AFA�AE�AD �AB��A@�A>�jA=�TA=l�A=�A<A�A;"�A:��A9dZA8 �A6�A5��A4��A3"�A2�RA2n�A1�-A0-A.��A-��A,��A+�hA*E�A(JA&��A&VA&5?A%oA#�;A"��A!XA ��A A��A33An�A�A�-A
=A�hA=qAA;dA��AAA�!A�#A�A��A�+AffA&�A1'A�AjAO�A1A
��A	�hAn�AK�A��A9XAv�A z�@�n�@��
@�"�@��h@��@�E�@�(�@�@�33@��@�O�@��@�u@�dZ@�$�@��@��m@�$�@�@�\)@��@���@�;d@��@޸R@���@�I�@�|�@��H@��@׶F@��@�M�@��@���@պ^@Ձ@��`@�I�@�+@�v�@�Ĝ@��;@�V@��@�1'@˝�@�C�@���@�j@ȃ@�1'@��@�\)@��@�?}@��`@�1'@�33@��h@�Z@�33@��!@���@�G�@�Q�@��@��h@���@��D@�b@��F@�
=@��@��y@�v�@��u@���@�dZ@���@�Ĝ@�I�@�(�@���@��R@��\@�V@�5?@���@�`B@��@�Ĝ@���@�r�@�9X@��@�|�@��@��H@��\@���@�7L@���@���@���@�I�@�ƨ@�S�@���@�n�@�E�@�-@�{@�@���@��h@�x�@�7L@���@��D@�A�@��
@�|�@�dZ@���@���@��@���@�&�@��`@�Q�@�  @�  @��F@�dZ@�33@�o@�@��@���@���@�@��-@��7@�`B@�7L@��@�V@���@���@��D@� �@�1@��@���@�
=@���@�~�@���@���@�@�?}@�V@���@��u@�Z@� �@��@��w@���@��@�S�@��@���@�J@���@�p�@�X@�/@�%@���@��u@�Z@�(�@���@��@��@���@�t�@�K�@�+@�
=@��@�n�@�E�@�J@��@��^@���@���@���@��7@�x�@�X@�?}@���@�j@�9X@�  @���@��F@���@�dZ@�o@���@�n�@�V@�V@�-@��@���@��@���@���@�Ĝ@��@��D@�r�@�Z@�b@���@�|�@�dZ@�dZ@�"�@�@��R@�v�@�E�@��@���@���@�@��-@��@�%@��9@�z�@�1'@�1'@��@��@���@��+@�$�@��@��T@��T@���@���@�p�@�&�@���@��`@��j@�r�@�j@�r�@�j@�Q�@�9X@�b@�I�@{��@q?}@h��@a \@Z�x@Tl"@M	l@F
�@?�V@7�]@2�@,Ft@&	@s@��@�+@��@�6@M@l�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A��hA���A���A���A���A��A���A���A���A��uA���A���A���A���A�z�A�jA�ffA�ffA�hsA�ffA�ffA�ffA�`BA�`BA�bNA�dZA�ffA�l�A�n�A�jA�ffA�hsA�^5A�I�A�{A���A���A�
=A�\)A���A�oA���A�p�A���A��DA��A�
=A�?}A�ZA��A��jA��`A�  A���A��A��
A�G�A��TA���A�A�A�A�9XA��HA��+A�hsA��A���A��-A�XA���A��wA�dZA��DA���A�{A�O�A��A���A�`BA��A�+A��A���A�E�A�\)A�$�A��TA�n�A��A���A�M�A��A��A��;A��A��A�dZA�+A��
A���A�"�A�A��A�
=A��hA�  A�z�A�(�A���A���A��A}XA|~�A{?}At=qAq�
An��AmK�Al�AhȴAghsAe�Ae+Ad�!AcƨAbn�Aa�7Aa33A`A_�TA_XA]�AZI�AW��AWK�AV�!AU\)AR�RAP��AO��AN�AM�AMK�AKl�AH�9AG�-AFA�AE�AD �AB��A@�A>�jA=�TA=l�A=�A<A�A;"�A:��A9dZA8 �A6�A5��A4��A3"�A2�RA2n�A1�-A0-A.��A-��A,��A+�hA*E�A(JA&��A&VA&5?A%oA#�;A"��A!XA ��A A��A33An�A�A�-A
=A�hA=qAA;dA��AAA�!A�#A�A��A�+AffA&�A1'A�AjAO�A1A
��A	�hAn�AK�A��A9XAv�A z�@�n�@��
@�"�@��h@��@�E�@�(�@�@�33@��@�O�@��@�u@�dZ@�$�@��@��m@�$�@�@�\)@��@���@�;d@��@޸R@���@�I�@�|�@��H@��@׶F@��@�M�@��@���@պ^@Ձ@��`@�I�@�+@�v�@�Ĝ@��;@�V@��@�1'@˝�@�C�@���@�j@ȃ@�1'@��@�\)@��@�?}@��`@�1'@�33@��h@�Z@�33@��!@���@�G�@�Q�@��@��h@���@��D@�b@��F@�
=@��@��y@�v�@��u@���@�dZ@���@�Ĝ@�I�@�(�@���@��R@��\@�V@�5?@���@�`B@��@�Ĝ@���@�r�@�9X@��@�|�@��@��H@��\@���@�7L@���@���@���@�I�@�ƨ@�S�@���@�n�@�E�@�-@�{@�@���@��h@�x�@�7L@���@��D@�A�@��
@�|�@�dZ@���@���@��@���@�&�@��`@�Q�@�  @�  @��F@�dZ@�33@�o@�@��@���@���@�@��-@��7@�`B@�7L@��@�V@���@���@��D@� �@�1@��@���@�
=@���@�~�@���@���@�@�?}@�V@���@��u@�Z@� �@��@��w@���@��@�S�@��@���@�J@���@�p�@�X@�/@�%@���@��u@�Z@�(�@���@��@��@���@�t�@�K�@�+@�
=@��@�n�@�E�@�J@��@��^@���@���@���@��7@�x�@�X@�?}@���@�j@�9X@�  @���@��F@���@�dZ@�o@���@�n�@�V@�V@�-@��@���@��@���@���@�Ĝ@��@��D@�r�@�Z@�b@���@�|�@�dZ@�dZ@�"�@�@��R@�v�@�E�@��@���@���@�@��-@��@�%@��9@�z�@�1'@�1'@��@��@���@��+@�$�@��@��T@��T@���@���@�p�@�&�@���@��`@��j@�r�@�j@�r�@�j@�Q�@�9XG�O�@�I�@{��@q?}@h��@a \@Z�x@Tl"@M	l@F
�@?�V@7�]@2�@,Ft@&	@s@��@�+@��@�6@M@l�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB\BVBVB\BVBVBVBVBVBPBPBVBVBPBPBPBPBPBPBVBVBVBVBVB\BbBbBhBuBoBhBuB�B�B(�B5?B<jBW
BiyBp�Bt�B� B� By�Br�Bp�Bl�BhsBdZB`BB\)BaHBbNBaHB]/BVBR�BO�BM�BJ�BG�BD�BB�B?}B7LB.B"�B�B�BVBB��B�B�B�sB�5B��B��B��BƨB�B�uB�DB�B�Bs�BdZBN�B;dB8RB8RB?}B;dB49B2-B/B-B'�B �BhB
�B
�BB
��B
ŢB
�^B
��B
��B
�JB
�B
t�B
ffB
W
B
M�B
:^B	��B	�`B	��B	ÖB	�^B	��B	��B	��B	�oB	�VB	�JB	�bB	�bB	�VB	�DB	�JB	�+B	|�B	o�B	_;B	]/B	XB	O�B	C�B	:^B	33B	.B	(�B	#�B	�B	PB		7B	B	  B��B�B�B�`B�NB�BB�5B�#B�B�B��B��BŢB��B�jB�XB�FB�9B�'B�B��B��B��B��B��B�uB�\B�PB�JB�DB�=B�7B�%B�B� B� B�B�B~�B}�B}�B�B�B~�B}�B{�By�Bw�Bu�Bt�Bt�Bs�Br�Bq�Bp�Bo�Bn�Bm�Bl�Bn�Bp�Bo�Bm�Bl�BjBhsBdZBcTBe`Bk�Bk�BiyBk�Br�Br�Bq�Bn�Bk�BiyBhsBffBbNBbNBffBgmBdZBiyBjBhsBiyBjBl�Bl�Bm�Bo�Bo�Bn�Bp�Br�Br�Bt�Bu�Bv�Bv�Bw�Bx�By�Bz�Bz�B~�B� B�B�B�%B�+B�1B�VB�oB��B��B��B��B��B��B��B��B��B�B�B�9B�FB�^B�dB�wBĜB��B��B��B��B��B�B�B�B�B�ZB�fB�yB�B��B	B	B	B	1B	
=B	JB	PB	\B	hB	uB	�B	�B	�B	�B	�B	�B	 �B	#�B	$�B	-B	2-B	6FB	7LB	8RB	:^B	=qB	@�B	D�B	F�B	G�B	G�B	H�B	L�B	M�B	N�B	O�B	Q�B	VB	XB	ZB	]/B	_;B	`BB	cTB	ffB	jB	m�B	r�B	s�B	v�B	w�B	x�B	y�B	{�B	|�B	}�B	}�B	~�B	� B	�B	�B	�1B	�=B	�VB	�\B	�bB	�bB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�-B	�3B	�3B	�3B	�3B	�9B	�FB	�LB	�RB	�^B	�dB	�qB	�wB	�}B	��B	B	ÖB	ŢB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�)B	�;B	�BB	�HB	�NB	�TB	�TB	�NB	�NB	�TB	�ZB	�`B	�`B	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
	7B
	7B

=B

=B

=B
JB
B
YB
�B
+�B
4�B
8lB
:^B
BAB
IRB
NpB
V�B
]/B
b�B
i*B
o B
t�B
x�B
{�B
~(B
�GB
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B �B��B��B �B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B �B�B�B�B�B�B�B�B�B�B=B&�B-�BHSBZ�Ba�Be�BqDBqDBkBc�Ba�B]�BY�BU�BQ�BMqBR�BS�BR�BNxBGQBD:BA(B?#B<B9B5�B3�B0�B(�BhB#B
�B�B��B�jB�!B�B��B��BϕB�[B�9B�)B�B�iB��B|�BsuBrqBe BU�B@HB,�B)�B)�B0�B,�B%�B#�B �B�BiB;B�B
�"B
��B
�^B
�)B
��B
�kB
�1B
}�B
s�B
fHB
W�B
H�B
?gB
+�B	�B	��B	�B	�9B	� B	��B	�YB	�0B	�B	�B	}�B	�B	�B	�B	|�B	}�B	x�B	n�B	aOB	P�B	N�B	I�B	A�B	5JB	,B	$�B	�B	�B	�B	
QB�B��B��B��B�B�uB�OB�%B�B�	B��B��B��B��B­B��B�iB�MB�6B�$B�B�B��B��B��B��B��B��B�xB�GB�+B B~B}B|B{	Bw�Bu�Bq�Bq�Br�Br�Bp�Bo�Bo�Br�Br�Bp�Bo�Bm�Bk�Bi�Bg�Bf�Bf�Be�Bd�Bc�Bb}Ba|B`uB_kB^gB`rBbBazB_nB^iB\ZBZNBV9BU2BW>B]cB]cB[XB]hBd�Bd�Bc�B`uB]gB[XBZRBXGBT.BT/BXCBYPBV<B[^B\aBZWB[YB\bB^oB^mB_nBa�Ba�B`xBb�Bd�Bd�Bf�Bg�Bh�Bh�Bi�Bj�Bk�Bl�Bl�Bp�Bq�Bs�Bu�BxBy	BzB�6B�NB�gB�jB�mB�xB�{B��B��B��B��B��B��B�B�"B�<B�?B�RB�zB��B��B��B��B��B��B��B��B��B�2B�?B�PB�B��B��B��B��B�B�B�B�%B	3B	9B	NB	[B		aB	kB	qB	B	�B	�B	�B	�B	�B	$B	(B	)B	*%B	,1B	/BB	2QB	6lB	8xB	9}B	9{B	:�B	>�B	?�B	@�B	A�B	C�B	G�B	I�B	K�B	N�B	QB	RB	U$B	X7B	\MB	_[B	d|B	e�B	h�B	i�B	j�B	k�B	m�B	n�B	o�B	o�B	p�B	q�B	r�B	v�B	y�B	|B	�B	�&B	�+B	�-B	�9B	�CB	�IB	�NB	�ZB	�\B	�dB	�wB	�yB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�"B	�)B	�7B	�=B	�CB	�JB	�QB	�ZB	�gB	�jB	�wB	��B	��B	��B	��B	§B	ñB	ĴB	ĳB	ĴB	ƾB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	� B	�B	�-B	�;B	�@B	�QB	�UB	�WB	�YB	�[B	�]B	�`B	�ZB	�cB	�pB	�uB	�vB	�vB	�}B	�}B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��G�O�B	��B
	B
�B
�B
&�B
*)B
,B
3�B
;
B
@*B
H�B
N�B
TVB
Z�B
`�B
f�B
j�B
mOB
o�B
u B
y�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.09 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9996(+/-0.0001), vertically averaged dS =-0.014(+/-0.002) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Map scales: x=6,3; y=2,1. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                            201906040941442019060409414420190604094144  AO  ARCAADJP                                                                    20180221180238    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20180221180238  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20180221180238  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190604094144  IP                  G�O�G�O�G�O�                