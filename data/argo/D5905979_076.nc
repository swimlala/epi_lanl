CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-19T17:09:11Z creation      
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200619170911  20220204114418  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               LA   AO  7662                            2C  D   APEX                            8312                            080318                          846 @ؽ��1   @ؽI���@7/\(��c��x���1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    LA   B   B   @�ff@�  A   A   A@  A`  A�  A�  A���A���A�33A�33A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C �C"�C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4y�D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<fD<� D=  D=� D=��D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DVfDV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� DtٚDy�{D��D�h D��{D�� D�'\D�_
D���D��D�"�D�\)D��RD�ҏD� RD�[�D�:�D��fD�
�D�K�D��D��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��A�\A>�\A^�\A~�\A�G�A�{A�{A�z�A�z�A�G�A�G�A�G�B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C �C"�C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C�HC��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{D z=D �=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=D	z=D	�=D
z=D
�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=D z=D �=D!z=D!�=D"z=D"�=D#z=D#�=D$z=D$�=D%z=D%�=D&z=D&�=D'z=D'�=D(z=D(�=D)z=D)�=D*z=D*�=D+z=D+�=D,z=D,�=D-z=D-�=D.z=D.�=D/z=D/�=D0z=D0�=D1z=D1�=D2z=D2�=D3z=D3�=D4s�D4�=D5z=D5�=D6z=D6�=D7z=D7�=D8z=D8�=D9z=D9�=D:z=D:�=D;z=D< �D<z=D<�=D=z=D=��D>z=D>�=D?z=D?�=D@z=D@�=DAz=DA�=DBz=DB�=DCz=DC�=DDz=DD�=DEz=DE�=DFz=DF�=DGz=DG�=DHz=DH�=DIz=DI�=DJz=DJ�=DKz=DK�=DLz=DL�=DMz=DM�=DNz=DN�=DOz=DO�=DPz=DP�=DQz=DQ�=DRz=DR�=DSz=DS�=DTz=DT�=DUz=DV �DVz=DV�=DWz=DW�=DXz=DX�=DYz=DY�=DZz=DZ�=D[z=D[�=D\z=D\�=D]z=D]�=D^z=D^�=D_z=D_�=D`z=D`�=Daz=Da�=Dbz=Db�=Dcz=Dc�=Ddz=Dd�=Dez=De�=Dfz=Df�=Dgz=Dg�=Dhz=Dh�=Diz=Di�=Djz=Dj�=Dkz=Dk�=Dlz=Dl�=Dmz=Dm�=Dnz=Dn�=Doz=Do�=Dpz=Dp�=Dqz=Dq�=Drz=Dr�=Dsz=Ds�=Dtz=Dt��Dy��D��D�eD���D��D�${D�\)D���D��3D�  D�YHD��qD�ϮD�qD�X�D�8 D�ӅD� D�H�D�D��>111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��`A��mA��A��A��;A��A��TA��yA��TA�ffA��A��HA��A��A�^5A�=qA��+A���A��jA�v�A���A��wA�bNA�ƨA�p�A�ƨA���A�;dA��\A�x�A�\)A��PA��yA�r�A�/A�%A���A��uA��HA���A���A�K�A�1'A�A�Q�A�K�A��;A���A��PA�z�A�l�A�bNA��yA��7A��A��DA�?}A�A���A��A��/A���A�33A��#A��hA�`BA�33A��A���A�-A��DA�$�A�r�A���A�A��A��A��A�|�A�33A�ȴA�G�A��FA�VA�t�A��A���A��A��9A�oA��A��A���A��A�\)A��A��DA�(�A���A���A�JA�ffA��A��A�1'A��A��#A�ZA�;dA�A�33A���A��A��^A��A~bNA}Az1Ax �Av �At��As;dAo�^Am�;Al�yAl�Aj�yAg�PAd�jAcG�Aa�A^�A]G�AZ�uAX�RAWx�AV~�AV-AUhsAT�RAR�jAP��ANbNAL��AK�PAK"�AJ��AJ(�AI�FAIl�AH-AFffAEK�ADz�AC�#AC;dA@~�A>��A=S�A;�A9��A9G�A8��A8E�A7��A6�DA5�mA5?}A4v�A3dZA2�9A29XA1oA/hsA-��A,ȴA+�wA*1'A)VA&^5A$�+A#G�A"�uA!��A ��A bA�
A�AoA�RAI�A�A33A��A��A{A�A�Av�A�wA(�AO�AAAVA\)A{A�!A��A7LA�TA
ĜA	K�A��AE�A�`A�A�A�RAt�A=qAJA�wA �\@��
@�5?@�|�@��-@��9@���@�33@�J@���@�n�@�K�@���@� �@���@�t�@�n�@�j@�^5@�V@�D@�dZ@�
=@��@�E�@���@���@�+@ޟ�@��/@�(�@�l�@�M�@��@�o@�;d@�+@�M�@���@���@�ff@�b@�ff@��m@�ƨ@�"�@ҏ\@��@�O�@�O�@��@�bN@�ȴ@�V@̓u@̛�@̋D@��
@�C�@�=q@ɑh@�G�@�Q�@�33@�ȴ@�5?@Ų-@�r�@��@å�@�
=@��@��7@�hs@�G�@��@��D@�9X@���@�l�@���@�E�@�?}@�V@�%@���@�  @�t�@�K�@�o@��y@�E�@���@�7L@��@�r�@�;d@�^5@�=q@��#@�O�@��j@�Z@�I�@�ƨ@���@���@��+@��@��@�b@���@��@��@���@��@��;@�ff@�$�@��@�z�@�j@�1'@��;@���@��P@���@���@���@��@��@��@�dZ@�ȴ@�M�@�hs@�V@���@��/@��u@�(�@�  @��F@�33@���@���@�~�@�$�@��@�bN@��P@�K�@�
=@��@���@��@���@���@��!@��\@�{@��@�X@�/@�%@��`@�r�@�1'@�1@��
@��@�S�@�"�@�+@�o@��H@��!@��\@�~�@�ff@�-@�@�@�O�@�&�@��@���@�Z@�(�@��@�1@��F@�dZ@�33@��@���@���@�^5@��@��T@��-@���@���@���@���@��7@�O�@�7L@�/@��@���@���@�I�@��
@���@���@�l�@�K�@�+@�
=@��@��R@��+@�^5@�-@�V@��\@��+@�~�@��R@�ȴ@���@�E�@��@��^@�X@�G�@��@���@�r�@�I�@��@��F@���@��@��@�|�@���@���@�E�@�-@�{@��@��#@���@�x�@��/@�9X@��@�ƨ@��@�\)@�C�@�"�@��R@�~�@�U�@|�@s�@f($@]/@S�
@M^�@G��@A�'@;�@5��@09X@)�#@%ϫ@ $@l�@�A@�M@8@�U@	T�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A��`A��mA��A��A��;A��A��TA��yA��TA�ffA��A��HA��A��A�^5A�=qA��+A���A��jA�v�A���A��wA�bNA�ƨA�p�A�ƨA���A�;dA��\A�x�A�\)A��PA��yA�r�A�/A�%A���A��uA��HA���A���A�K�A�1'A�A�Q�A�K�A��;A���A��PA�z�A�l�A�bNA��yA��7A��A��DA�?}A�A���A��A��/A���A�33A��#A��hA�`BA�33A��A���A�-A��DA�$�A�r�A���A�A��A��A��A�|�A�33A�ȴA�G�A��FA�VA�t�A��A���A��A��9A�oA��A��A���A��A�\)A��A��DA�(�A���A���A�JA�ffA��A��A�1'A��A��#A�ZA�;dA�A�33A���A��A��^A��A~bNA}Az1Ax �Av �At��As;dAo�^Am�;Al�yAl�Aj�yAg�PAd�jAcG�Aa�A^�A]G�AZ�uAX�RAWx�AV~�AV-AUhsAT�RAR�jAP��ANbNAL��AK�PAK"�AJ��AJ(�AI�FAIl�AH-AFffAEK�ADz�AC�#AC;dA@~�A>��A=S�A;�A9��A9G�A8��A8E�A7��A6�DA5�mA5?}A4v�A3dZA2�9A29XA1oA/hsA-��A,ȴA+�wA*1'A)VA&^5A$�+A#G�A"�uA!��A ��A bA�
A�AoA�RAI�A�A33A��A��A{A�A�Av�A�wA(�AO�AAAVA\)A{A�!A��A7LA�TA
ĜA	K�A��AE�A�`A�A�A�RAt�A=qAJA�wA �\@��
@�5?@�|�@��-@��9@���@�33@�J@���@�n�@�K�@���@� �@���@�t�@�n�@�j@�^5@�V@�D@�dZ@�
=@��@�E�@���@���@�+@ޟ�@��/@�(�@�l�@�M�@��@�o@�;d@�+@�M�@���@���@�ff@�b@�ff@��m@�ƨ@�"�@ҏ\@��@�O�@�O�@��@�bN@�ȴ@�V@̓u@̛�@̋D@��
@�C�@�=q@ɑh@�G�@�Q�@�33@�ȴ@�5?@Ų-@�r�@��@å�@�
=@��@��7@�hs@�G�@��@��D@�9X@���@�l�@���@�E�@�?}@�V@�%@���@�  @�t�@�K�@�o@��y@�E�@���@�7L@��@�r�@�;d@�^5@�=q@��#@�O�@��j@�Z@�I�@�ƨ@���@���@��+@��@��@�b@���@��@��@���@��@��;@�ff@�$�@��@�z�@�j@�1'@��;@���@��P@���@���@���@��@��@��@�dZ@�ȴ@�M�@�hs@�V@���@��/@��u@�(�@�  @��F@�33@���@���@�~�@�$�@��@�bN@��P@�K�@�
=@��@���@��@���@���@��!@��\@�{@��@�X@�/@�%@��`@�r�@�1'@�1@��
@��@�S�@�"�@�+@�o@��H@��!@��\@�~�@�ff@�-@�@�@�O�@�&�@��@���@�Z@�(�@��@�1@��F@�dZ@�33@��@���@���@�^5@��@��T@��-@���@���@���@���@��7@�O�@�7L@�/@��@���@���@�I�@��
@���@���@�l�@�K�@�+@�
=@��@��R@��+@�^5@�-@�V@��\@��+@�~�@��R@�ȴ@���@�E�@��@��^@�X@�G�@��@���@�r�@�I�@��@��F@���@��@��@�|�@���@���@�E�@�-@�{@��@��#@���@�x�@��/@�9X@��@�ƨ@��@�\)@�C�@�"�@��RG�O�@�U�@|�@s�@f($@]/@S�
@M^�@G��@A�'@;�@5��@09X@)�#@%ϫ@ $@l�@�A@�M@8@�U@	T�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBBBBBBBBBB	7B@�BbNBhsBy�B��B�-B�BYBx�B�%B�B}�B�JB��B��B��B��B�JB�=B�1B�7B�PB�hB��B��B��B��B��B�B�B�'B�9B�?B�^B��BȴBƨBĜBÖBÖBŢBŢBÖB��B�#B�TB�mB�B�B�yB�mB�NB�HB�fB�B�B�B�B�B�mB�B�B��B�1Bt�Bp�Bp�B{�B��B�'B�B��B��B��Bp�Bq�Bw�BW
BC�B6FB<jBYBI�BH�B)�BƨB�-B� B^5BVBO�BG�B;dB33B�BJB
��B
�B
�B
�ZB
�B
�^B
�B
��B
�1B
x�B
p�B
aHB
O�B
F�B
9XB
2-B
 �B
bB

=B
B	��B	�yB	�B	��B	ĜB	�-B	��B	��B	�hB	�DB	�B	�B	}�B	w�B	p�B	cTB	ZB	O�B	J�B	F�B	B�B	>wB	:^B	7LB	2-B	&�B	 �B	�B	�B	bB	B��B��B�B�fB�TB�TB�NB�HB�;B�#B�B��B��BŢB��B�qB�FB�B��B��B��B�{B�DB�B{�Bx�Bv�Bt�Bq�Bp�Bp�Bo�Bo�Bo�Bm�Bm�Bk�BiyBhsBgmBhsBdZBbNB_;B`BB[#BZBZBYBW
BS�BQ�BP�BN�BM�BL�BI�BJ�BH�BG�BE�BE�BC�BC�BA�BA�B@�B>wB>wB<jB9XB8RB8RB7LB6FB6FB8RB8RB:^B8RB7LB7LB5?B49B7LB8RB8RB8RB8RB7LB8RB9XB<jB@�BB�B@�BK�BN�BN�BN�BJ�BP�BR�BQ�BO�BQ�B^5BbNB[#Bl�Bu�Bv�Bw�B� B�B�+B�1B�7B�1B�1B�=B�JB�VB�hB�oB��B��B��B��B��B��B��B��B��B��B��B��B�3B�FB�LB�LB�RB�qB�wB�}BBŢB��B��B��B��B�
B�)B�BB�HB�TB�ZB�yB�B�B�B��B	B	B	B	1B	JB	bB	{B	�B	�B	�B	"�B	%�B	+B	-B	.B	2-B	/B	,B	)�B	%�B	!�B	�B	�B	�B	�B	�B	!�B	#�B	(�B	0!B	6FB	7LB	8RB	9XB	<jB	A�B	D�B	F�B	F�B	G�B	G�B	G�B	G�B	G�B	G�B	G�B	G�B	H�B	H�B	H�B	I�B	J�B	L�B	O�B	O�B	O�B	R�B	T�B	W
B	ZB	\)B	]/B	]/B	^5B	`BB	aHB	aHB	bNB	dZB	e`B	gmB	jB	l�B	n�B	p�B	t�B	t�B	u�B	v�B	w�B	x�B	y�B	z�B	{�B	}�B	�B	�B	�B	�7B	�JB	�bB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�!B	�!B	�-B	�3B	�9B	�9B	�9B	�?B	�RB	�^B	�jB	�qB	�}B	��B	��B	B	B	��B	��B	��B	��B	��B	B	ÖB	ƨB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�B	�B	�B	�B	�B	��B	��B
�B
B
 �B
-wB
3�B
9	B
?�B
FYB
KxB
O�B
WsB
Z�B
_;B
c�B
g�B
lB
p�B
tB
x�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B
�jB
�jB
�qB
�qB
�qB
�qB
�qB
�qB
�jB�B8�BZ�B`�Br.B��B�}B��BQ]BqB~hB{VBv8B��B��B�B��B��B��B��B�uB�|B��B��B��B��B��B��B�3B�^B�^B�jB�|B��B��B�B��B��B��B��B��B��B��B��B�4B�dBەB߮B��B�B�B߮BڏBىBާB��B��B��B��B��B߮B�_B�MB��B�xBmBh�Bh�Bt/B�B�lB�`B�6B�B��Bh�Bi�BpBOUB;�B.�B4�BQcBBBAB"KB��B��BxYBV�BN`BH<B@B3�B+�B�B�B
�>B
��B
��B
ܾB
҂B
��B
�|B
�B
��B
qAB
iB
Y�B
HNB
?B
1�B
*�B
7B
�B
�B	��B	�cB	��B	юB	�EB	�B	��B	�qB	�.B	��B	��B	|�B	z�B	vrB	pNB	i#B	[�B	R�B	HaB	CCB	?*B	;B	6�B	2�B	/�B	*�B	nB	JB	%B	B	�B��B�DB�WB�!B��B��B��B��B��B��BӮBΏB�wB�MB�.B�B��B��B��B�lB�TB�B�B��Bz�BtyBqgBo[BmOBj=Bi7Bi7Bh1Bh1Bh1Bf$Bf$BdBbBaB`BaB\�BZ�BW�BX�BS�BR�BR�BQ�BO�BL�BJ�BI|BGpBFjBEdBBRBCYBALB@FB>:B>:B</B</B:"B:"B9B7B7B5B1�B0�B0�B/�B.�B.�B0�B0�B2�B0�B/�B/�B-�B,�B/�B0�B0�B0�B0�B/�B0�B1�B5B9B;*B9BDbBGtBGtBGtBC\BI�BK�BJ�BHzBJ�BV�BZ�BS�Be$Bn\BobBphBx�B|�B�B��B��B��B��B��B��B��B� B�B�B�B�B�+B�+B�+B�+B�2B�IB�\B�hB��B��B��B��B��B��B�B�B�B�%B�8B�\B̍B͓B͓BϟBԽB��B��B��B��B�B�B�7B�CB�bB��B��B��B	 �B	�B	�B	B	B	1B	IB	bB	tB	#�B	%�B	&�B	*�B	'�B	$�B	"�B	tB	]B	JB	DB	DB	JB	PB	]B	iB	!�B	(�B	.�B	/�B	0�B	1�B	4�B	:B	=,B	?8B	?8B	@>B	@>B	@>B	@>B	@>B	@>B	@>B	@>B	ADB	ADB	ADB	BJB	CQB	E]B	HoB	HoB	HoB	K�B	M�B	O�B	R�B	T�B	U�B	U�B	V�B	X�B	Y�B	Y�B	Z�B	\�B	]�B	_�B	cB	eB	g&B	i2B	mJB	mJB	nQB	oWB	p]B	qcB	riB	soB	tuB	v�B	y�B	z�B	}�B	��B	��B	��B	�B	�B	�B	�B	�&B	�2B	�2B	�>B	�DB	�JB	�QB	�]B	�cB	�oB	�oB	�uB	�uB	�uB	�|B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	� B	�2B	�8B	�>B	�DB	�KB	�WB	�]B	�cB	�cB	�iB	�oB	�vB	�vB	�vB	�|B	̂B	͈B	΍B	ϓB	ϓB	ЙB	ЙB	ЙB	ЙG�O�B	݀B	�"B	�xB
�B
fB
%�B
,"B
1�B
8RB
>�B
C�B
HJB
O�B
S?B
W�B
\BB
`&B
d�B
iB
l�B
q?111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.09 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9998(+/-0.0001), vertically averaged dS =-0.007(+/-0.003) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144182022020411441820220204114418  AO  ARCAADJP                                                                    20200619170911    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200619170911  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200619170911  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114418  IP                  G�O�G�O�G�O�                