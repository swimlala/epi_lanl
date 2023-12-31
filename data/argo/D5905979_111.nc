CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-19T17:09:19Z creation      
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
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �0   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �@   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �D   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �T   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �X   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �\   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �`Argo profile    3.1 1.2 19500101000000  20200619170919  20220204114422  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               oA   AO  7662                            2C  D   APEX                            8312                            080318                          846 @���nl1   @����i�@6�7Kƨ�c�t�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    oA   B   B   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�33B�  B�  B�  B�  B�  B�  B���B�ffB�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C �C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C?�fCB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� DlfDl�fDm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dts3DyۅD� D�]D���D���D��D�]D��{D�߮D�\D��qD���D��qD��D�Q�Dګ3D�޸D�RD�O�D�D�ʏ11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��A�\A>�\A^�\A~�\A�G�A�G�A�G�A�G�A�G�A�G�A�G�A�G�B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B���B�B���B���B���B���B���B���B���B�8RB���B���B���B�B���B���B���B���B���B���B���B���B���B�B���B���B���B���B���B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C �C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?�\CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C�HC��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��C��C��{C��{C��{C��{C��{C��{C��{C��{C��{C�HC��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{D z=D �=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D��Dz=D�=Dz=D�=Dz=D�=D	z=D	�=D
z=D
�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D �Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=D z=D �=D!z=D!�=D"z=D"�=D#z=D#�=D$z=D$�=D%z=D%�=D&z=D&�=D'z=D'�=D(z=D(�=D)z=D)�=D*z=D*�=D+z=D+�=D,z=D,�=D-z=D-�=D.z=D.�=D/z=D/�=D0z=D0�=D1z=D1�=D2z=D2�=D3z=D3�=D4z=D4�=D5z=D5�=D6z=D6�=D7z=D7�=D8z=D8�=D9z=D9�=D:z=D:�=D;z=D;�=D<z=D<�=D=z=D=�=D>z=D>�=D?z=D?�=D@z=D@�=DAz=DA�=DBz=DB�=DCz=DC�=DDz=DD�=DEz=DE�=DFz=DF�=DGz=DG�=DHz=DH�=DIz=DI�=DJz=DJ�=DKz=DK�=DLz=DL�=DMz=DM�=DNz=DN�=DOz=DO�=DPz=DP�=DQz=DQ�=DRz=DR�=DSz=DS�=DTz=DT�=DUz=DU�=DVz=DV�=DWz=DW�=DXz=DX�=DYz=DY�=DZz=DZ�=D[z=D[�=D\z=D\�=D]z=D]�=D^z=D^�=D_z=D_�=D`z=D`�=Daz=Da�=Dbz=Db�=Dcz=Dc�=Ddz=Dd�=Dez=De�=Dfz=Df�=Dgz=Dg�=Dhz=Dh�=Diz=Di�=Djz=Dj�=Dkz=Dl �Dl��Dl�=Dmz=Dm�=Dnz=Dn�=Doz=Do�=Dpz=Dp�=Dqz=Dq�=Drz=Dr�=Dsz=Ds�=DtmpDy��D�D�Z>D���D��D�D�Z>D���D���D�{D��D��D�ҐD��D�ODڨRD���D�qD�L�D��D�Ǯ11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AضFAضFAذ!Aز-A؍PA�ZA�E�A�5?A�/A�+A�+A�(�A�$�A� �A��A��A� �A� �A��A�bA�VA�  A���A��A��A��mA��mA��`A��TA��A���A���A���A��A���A���A���A��A�$�AӍPA��A�  A�-A���AƮA©�A�-A�A��\A���A���A�%A���A�A�33A�VA�1A�VA���A�ZA�r�A���A�
=A�`BA�z�A� �A�G�A��`A�hsA�v�A���A�dZA���A�K�A���A��mA��7A���A�jA�VA�A�A���A�K�A���A���A�A��A��DA�C�A�$�A��A��PA��A��TA�E�A���A�;dA�K�A��A��+A��A�1A�ffA�  A��uA��#A��hA���A�/A���A�O�A��A~v�A{;dAvbAqS�An��Am��Al9XAj�!Ai`BAgx�Ae�Ad�DAc;dAaG�A^�/A]7LA[C�AZJAX��AWVAUC�AS7LAR�AQC�AO�AK��AHbNAFn�ADz�AB�A?�A<�HA<��A;t�A:JA9hsA8��A8  A7K�A6r�A61'A5ƨA4�\A2A/�A,�jA,{A+A++A)l�A($�A&1'A$��A#"�A!�A ��A�mAC�A��A��AĜA�A�A��A��A�wA�9AA��A33A�\A�hA��A�wA��A�A?}A�AbAO�A?}A=qA9XA�
A��A7LA�HA �A�AVA\)A	�
A��A��AI�A��Ap�A��AC�A��Al�A v�@�~�@�%@���@��@�Ĝ@���@��@��u@�v�@�I�@��@�ff@�@�@���@��`@�+@�E�@�{@��@�{@噚@�u@�@��#@��@�O�@�(�@��@ܣ�@�;d@�-@׾w@֗�@Ցh@Դ9@��@�;d@���@�  @���@Ɂ@ǝ�@��@�V@�Z@Å@�;d@�-@�G�@��@�Q�@��P@�A�@�  @���@�l�@���@�5?@�-@���@���@��\@��+@��T@���@��@��@��^@�r�@��
@��P@�n�@���@�b@���@�@�ff@�^5@�^5@�V@�V@���@�hs@��9@��T@���@���@�K�@�(�@��R@�5?@�ff@�|�@�M�@�9X@�Ĝ@���@���@�hs@� �@��P@��@�E�@��@���@��@�v�@��@���@��-@�x�@��-@��9@�  @��@��@�1@���@�9X@�l�@�9X@�Z@�  @��@���@���@��@�j@��u@�z�@�t�@�@��@��@�j@��+@��\@���@�5?@���@��^@��^@�b@��u@���@�M�@���@��@���@��!@��+@��@��@���@���@�C�@��m@��w@�l�@���@��-@�E�@�-@�G�@�r�@�A�@�(�@��@�(�@�  @���@��F@��;@���@���@�K�@�
=@��@��H@��R@�^5@�V@�-@�@�@��-@���@��@�`B@�G�@�/@��@��`@��9@�r�@�1'@��m@�ƨ@���@��P@�l�@�C�@�o@���@��+@�n�@�$�@�J@���@���@��h@�hs@�hs@�O�@���@��/@���@�A�@�b@��@��@���@�S�@�33@��y@��H@��H@��H@���@��@��T@�@��-@��7@�p�@�hs@�X@�&�@��`@��j@���@��@�I�@��@��w@�|�@�C�@�o@��!@�V@�-@���@��^@��-@���@�p�@���@��j@��j@���@�Q�@�9X@��@��@;d@l�@�P@�@~V@~{@}�T@}�-@}�@{�@u��@m�@gH�@_�@W�4@O�@H?�@?J#@:-@44n@/�@,1@(	�@#�$@�"@E�@p�@�.@;�@	�t11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   AضFAضFAذ!Aز-A؍PA�ZA�E�A�5?A�/A�+A�+A�(�A�$�A� �A��A��A� �A� �A��A�bA�VA�  A���A��A��A��mA��mA��`A��TA��A���A���A���A��A���A���A���A��A�$�AӍPA��A�  A�-A���AƮA©�A�-A�A��\A���A���A�%A���A�A�33A�VA�1A�VA���A�ZA�r�A���A�
=A�`BA�z�A� �A�G�A��`A�hsA�v�A���A�dZA���A�K�A���A��mA��7A���A�jA�VA�A�A���A�K�A���A���A�A��A��DA�C�A�$�A��A��PA��A��TA�E�A���A�;dA�K�A��A��+A��A�1A�ffA�  A��uA��#A��hA���A�/A���A�O�A��A~v�A{;dAvbAqS�An��Am��Al9XAj�!Ai`BAgx�Ae�Ad�DAc;dAaG�A^�/A]7LA[C�AZJAX��AWVAUC�AS7LAR�AQC�AO�AK��AHbNAFn�ADz�AB�A?�A<�HA<��A;t�A:JA9hsA8��A8  A7K�A6r�A61'A5ƨA4�\A2A/�A,�jA,{A+A++A)l�A($�A&1'A$��A#"�A!�A ��A�mAC�A��A��AĜA�A�A��A��A�wA�9AA��A33A�\A�hA��A�wA��A�A?}A�AbAO�A?}A=qA9XA�
A��A7LA�HA �A�AVA\)A	�
A��A��AI�A��Ap�A��AC�A��Al�A v�@�~�@�%@���@��@�Ĝ@���@��@��u@�v�@�I�@��@�ff@�@�@���@��`@�+@�E�@�{@��@�{@噚@�u@�@��#@��@�O�@�(�@��@ܣ�@�;d@�-@׾w@֗�@Ցh@Դ9@��@�;d@���@�  @���@Ɂ@ǝ�@��@�V@�Z@Å@�;d@�-@�G�@��@�Q�@��P@�A�@�  @���@�l�@���@�5?@�-@���@���@��\@��+@��T@���@��@��@��^@�r�@��
@��P@�n�@���@�b@���@�@�ff@�^5@�^5@�V@�V@���@�hs@��9@��T@���@���@�K�@�(�@��R@�5?@�ff@�|�@�M�@�9X@�Ĝ@���@���@�hs@� �@��P@��@�E�@��@���@��@�v�@��@���@��-@�x�@��-@��9@�  @��@��@�1@���@�9X@�l�@�9X@�Z@�  @��@���@���@��@�j@��u@�z�@�t�@�@��@��@�j@��+@��\@���@�5?@���@��^@��^@�b@��u@���@�M�@���@��@���@��!@��+@��@��@���@���@�C�@��m@��w@�l�@���@��-@�E�@�-@�G�@�r�@�A�@�(�@��@�(�@�  @���@��F@��;@���@���@�K�@�
=@��@��H@��R@�^5@�V@�-@�@�@��-@���@��@�`B@�G�@�/@��@��`@��9@�r�@�1'@��m@�ƨ@���@��P@�l�@�C�@�o@���@��+@�n�@�$�@�J@���@���@��h@�hs@�hs@�O�@���@��/@���@�A�@�b@��@��@���@�S�@�33@��y@��H@��H@��H@���@��@��T@�@��-@��7@�p�@�hs@�X@�&�@��`@��j@���@��@�I�@��@��w@�|�@�C�@�o@��!@�V@�-@���@��^@��-@���@�p�@���@��j@��j@���@�Q�@�9X@��@��@;d@l�@�P@�@~V@~{@}�T@}�-G�O�@{�@u��@m�@gH�@_�@W�4@O�@H?�@?J#@:-@44n@/�@,1@(	�@#�$@�"@E�@p�@�.@;�@	�t11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
�B
�B
�B
�B
�B
� B
�B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�+B
�1B
�+B
�%B
�B
�B
�B
�B
�B
�+B
�PB
��B
��B
��B
�B
�9B
�qB
ǮB
�B
��BL�B\)B�PBw�Bn�BhsBm�B�B�dB��B��B=qB>wB�B�B��BɺBȴBÖBǮB��B�RB��B�9BŢB�B49BM�BS�BW
B\)B_;BYBO�BQ�BVBS�BP�BI�BB�B@�B:^B/B�B��B�B�5BƨB�RB�9B�!B�B�oBx�B� B~�B�B|�BhsB_;BJ�BB�B1'B#�B�B1B
�fB
��B
�^B
�'B
��B
�{B
n�B
L�B
0!B
1B	�/B	ɺB	ȴB	��B	�RB	�B	��B	��B	�\B	�%B	y�B	l�B	dZB	]/B	VB	S�B	I�B	D�B	6FB	/B	)�B	�B	
=B��B�B�BB�B��B�^B�RB�?B�B�B��B��B��B��B��B��B��B��B�hB�DB�+B�%B�B~�By�Bs�Bl�BhsB`BB^5B\)B[#BYBVBW
BXBW
BYBYB]/B\)B\)B[#B\)B]/B^5B[#BZBVBVBZB]/BZBW
B\)Bw�B�B�B�%B�+B�=B�+B�B�B�Bt�Bl�BffB`BBZB\)BYBZB^5BR�BQ�BL�BK�BH�BG�BI�BK�BL�BJ�BG�BF�BL�BL�BT�BP�BM�BR�BK�BI�BI�BS�BR�BW
B[#B\)B]/B\)BbNBaHB^5B\)B\)B[#BYB^5Be`BhsBgmBffBffB^5BZBW
BYB\)BZB]/B]/B^5BaHB_;B_;B`BBbNBgmBm�Bm�Bw�Bw�Bv�Bz�Bz�B|�Bz�B|�B~�B� B�B�1B�VB�uB�oB�oB�uB��B��B��B��B��B��B��B��B��B��B�B��B�-B�XB�B�}B�^B�9B�9B�RBĜBŢB�}BǮB��B��B��B��B��B�HB�)B�
B��B��B�B�B�#B�)B�;B�TB�`B�yB�B��B��B	%B	1B	JB	oB	�B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	"�B	$�B	-B	7LB	49B	49B	:^B	9XB	8RB	;dB	=qB	K�B	R�B	VB	N�B	P�B	M�B	N�B	\)B	`BB	_;B	]/B	]/B	^5B	aHB	ffB	gmB	e`B	dZB	cTB	ffB	iyB	gmB	hsB	iyB	k�B	m�B	r�B	s�B	s�B	s�B	v�B	z�B	{�B	|�B	~�B	� B	� B	�B	�B	�+B	�7B	�=B	�PB	�PB	�VB	�\B	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�'B	�-B	�9B	�9B	�?B	�LB	�LB	�^B	�jB	�qB	�qB	�}B	�}B	B	ÖB	ƨB	ǮB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�B	�B	�)B	�)B	�/B	�/B	�5B	�;B	�BB	�BB	�HB	�HB	�TB	�TB	�ZB	�ZB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	�B	��B	��B
MB
�B
OB
(�B
/�B
9	B
@4B
F�B
KDB
P�B
UB
XEB
[�B
_�B
fB
j�B
o B
tnB
w2B
{d11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B
v^B
v^B
v^B
v^B
v^B
uXB
v^B
uXB
v^B
v^B
v^B
wdB
wdB
xjB
xjB
xjB
xjB
xjB
zwB
|�B
}�B
|�B
{}B
zwB
zwB
zwB
zwB
zwB
|�B
��B
��B
�!B
�>B
�cB
��B
��B
�B
�hB
�HBBBQqB��BmBc�B]�Bb�BvNB��B�3B� B2�B3�B�B�ZB�B��B��B��B��B�+B��B�B��B��B��B)xBCBI4BLFBQeBTwBNSBEBG)BKABI5BF#B>�B7�B5�B/�B$\B�B�?B��B�~B��B��B��B�nB�UB��Bn'BuRBtLBxdBr@B]�BT�B@B7�B&�B2B�B
��B
��B
�6B
��B
��B
�IB
��B
dB
B<B
%�B	��B	ҨB	�5B	�/B	�B	��B	��B	�VB	�B	��B	{�B	o^B	bB	Y�B	R�B	K�B	IB	?BB	:%B	+�B	$�B	�B	KB��B�MB�#B��B˘B�hB��B��B��B��B��B��B��B�wB�kB�XB�RB�;B�#B�B��B|�B{�Bx�Bt�BozBiVBb,B^BU�BS�BQ�BP�BN�BK�BL�BM�BL�BN�BN�BR�BQ�BQ�BP�BQ�BR�BS�BP�BO�BK�BK�BO�BR�BO�BL�BQ�BmrBw�Bz�B{�B|�B�B|�Bz�By�Bw�Bj`Bb0B\BU�BO�BQ�BN�BO�BS�BH�BG�BBvBApB>]B=XB?dBApBBwB@kB=XB<SBBwBBwBJ�BF�BC}BH�BArB?eB?eBI�BH�BL�BP�BQ�BR�BQ�BW�BV�BS�BQ�BQ�BP�BN�BS�B[
B^B]B\B\BS�BO�BL�BN�BQ�BO�BR�BR�BS�BV�BT�BT�BU�BW�B]Bc<Bc<BmzBmzBltBp�Bp�Br�Bp�Br�Bt�Bu�Bx�B}�B� B�B�B�B�B�*B�6B�CB�IB�UB�nB�tB��B��B��B��B��B��B��B��B�$B�B��B��B��B�BB�HB�$B�TBƋBȗBɝBȘBȘB��B��B̰BʤBʤBνB��B��B��B��B��B�B�B�ZB�mB�B��B��B	�B	B	"B	.B	.B	.B	"B	.B	.B	AB	_B	fB	qB	}B	"�B	,�B	)�B	)�B	/�B	.�B	-�B	1B	3B	AdB	H�B	K�B	DvB	F�B	CpB	DvB	Q�B	U�B	T�B	R�B	R�B	S�B	V�B	\B	]B	Z�B	Y�B	X�B	\B	_B	]B	^B	_B	a B	c,B	hKB	iQB	iQB	iQB	lcB	p{B	q�B	r�B	t�B	u�B	u�B	w�B	x�B	|�B	~�B	�B	��B	��B	��B	��B	�B	�B	�B	�%B	�>B	�DB	�JB	�UB	�hB	�tB	�zB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�$B	�+B	�=B	�CB	�CB	�IB	�[B	�gB	�gB	�mB	ǀB	ȆB	̞B	ͤB	ͤB	ΪB	ΪB	ϰB	ϰB	ѼB	ѼB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	�B	�B	�#B	�B	�B	�B	�B	�B	�B	�*B	�<B	�<B	�HB	�HB	�NB	�NB	�NB	�NB	�HG�O�B	�<B	��B
'B
�B
B
%-B
.�B
5�B
<�B
@�B
F%B
J�B
M�B
QMB
U�B
[�B
`uB
d�B
i�B
l�B
p�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.09 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9997(+/-0.0001), vertically averaged dS =-0.01(+/-0.004) in PSS-78.                                                                                                                                                                                        Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144222022020411442220220204114422  AO  ARCAADJP                                                                    20200619170919    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200619170919  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200619170919  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114422  IP                  G�O�G�O�G�O�                