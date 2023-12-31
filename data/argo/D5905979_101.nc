CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-19T17:09:17Z creation      
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200619170917  20220204114421  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               eA   AO  7662                            2C  D   APEX                            8312                            080318                          846 @��^]�ab1   @��^���@@7}�-V�cN^5?|�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    eA   B   B   @���@���@���A   A@  A`  A�  A�  A�  A�  A�  A�33A�33A�33B   B  B  B  B   B(  B0ffB8ffB@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B���B���B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C�C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C;�fC>  C@  CB  CD  CF  CH�CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt�fDu  Dy��D�"=D�YHD��qD��RD�
�D�T�D���D���D�\D�Y�D��RD�޸D� �D�]Dڛ�D��qD��D�R�D�}D��=111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��A�\A>�\A^�\A~�\A�G�A�G�A�G�A�G�A�z�A�z�A�z�A�G�B��B��B��B��B'��B0
=B8
=B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B���B���B�k�B�k�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B�B�B۞�Bߞ�B���B���B���B���B���B���B���B���C��C��C��C��C	��C��C��C��C��C�C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;�\C=��C?��CA��CC��CE��CH�CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��{C��{C��{C��{C��{C��{C��{C�HC��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C�HC��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��C��C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{D z=D �=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=D	z=D	�=D
z=D
�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=D z=D �=D!z=D!�=D"z=D"�=D#z=D#�=D$z=D$�=D%z=D%�=D&z=D&�=D'z=D'�=D(z=D(�=D)z=D)�=D*z=D*�=D+z=D+�=D,z=D,�=D-z=D-�=D.z=D.�=D/z=D/�=D0z=D0�=D1z=D1�=D2z=D2�=D3z=D3�=D4z=D4�=D5z=D5�=D6z=D6�=D7z=D7�=D8z=D8�=D9z=D9�=D:z=D:�=D;z=D;�=D<z=D<�=D=z=D=�=D>z=D>�=D?z=D?�=D@z=D@�=DAz=DA�=DBz=DB�=DCz=DC�=DDz=DD�=DEz=DE�=DFz=DF�=DGz=DG�=DHz=DH�=DIz=DI�=DJz=DJ�=DKz=DK�=DLz=DL�=DMz=DM�=DNz=DN�=DOz=DO�=DPz=DP�=DQz=DQ�=DRz=DR�=DSz=DS�=DTz=DT�=DUz=DU�=DVz=DV�=DWz=DW�=DXz=DX�=DYz=DY�=DZz=DZ�=D[z=D[�=D\z=D\�=D]z=D]�=D^z=D^�=D_z=D_�=D`z=D`�=Daz=Da�=Dbz=Db�=Dcz=Dc�=Ddz=Dd�=Dez=De�=Dfz=Df�=Dgz=Dg�=Dhz=Dh�=Diz=Di�=Djz=Dj�=Dkz=Dk�=Dlz=Dl�=Dmz=Dm�=Dnz=Dn�=Doz=Do�=Dpz=Dp�=Dqz=Dq�=Drz=Dr�=Dsz=Ds�=Dt��Dt�=Dy�3D�\D�VgD���D��qD� D�Q�D���D��D�{D�V�D��qD���D�D�Z>Dژ�D��D��D�P D�z>D�\111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�A�  A�  A�  A�A�A�A�%A�
=A�1A�
=A�
=A�JA�JA�1A�A���A���A��A��A��A���A���A��A��A��A��A��A��A��A��A���A�(�A�7LAԴ9A�jA�dZA�\)A��A�A���A�XA��HA�\)AɬA�|�A�M�A���A�E�AǇ+A��A�5?A��A�1A¶FA�A��A�n�A��FA�33A���A�{A��A�K�A�M�A���A�%A��9A�7LA���A��^A�A��A��A� �A��uA�^5A��A�n�A��7A�%A��RA�t�A�l�A���A�E�A�dZA�~�A��A�$�A��A��A�S�A�33A�ƨA�\)A�&�A���A�7LA�;dA���A�Q�A��^A�S�A�ffA�C�A��TA�VA�S�A���A��FA��A��A�x�A�5?A��A��
A�|�A��-A���A�p�A�jA�/A���A�-A�r�A���A~~�Ax�9Aut�As�-Ap��AmdZAlA�Aj�Ahn�Ag��Ag��Ag�Ae�Acl�AahsA_A^r�A]��A]7LA\1AY��AWdZAU33AS��ARn�AP��AM;dAJAH�uAD��A?�FA:��A7��A69XA5�
A5��A5x�A5dZA5K�A5%A4M�A3�PA2ĜA21A0�9A//A.�HA.��A-�^A+�A)�A(��A(bA%�-A$�A"��A �/A �uA n�A 5?A�FA�DA&�A��An�A^5AM�A�A�A9XAx�A=qA?}AZA�TA��AoA^5A�Av�A�A�A��A{A��A�wA�A
��A	`BA��AM�A�A�wA�PAG�A�`A^5A�^A"�A ��A ��A �D@�ƨ@��\@�ff@���@��/@��@�Z@�1@�K�@���@�  @��m@�S�@�/@��@�V@�@�7@��@�@�@�o@柾@��#@�%@�$�@ى7@�+@�E�@�x�@�b@�S�@�ȴ@�n�@�x�@���@���@�t�@���@ΰ!@�^5@͑h@���@�9X@ʏ\@ɡ�@�O�@�V@�9X@ǥ�@�+@�V@őh@�?}@���@ļj@ě�@Õ�@+@�M�@�@�@�@�@���@���@�I�@�Z@�dZ@�+@�ȴ@���@���@��9@�1@�C�@��y@�`B@���@�%@�j@�1'@�  @�j@�+@��@��@�33@�@�33@�;d@�n�@�ff@�ff@��@�O�@��@��@�{@��7@���@�&�@���@��T@��-@�p�@�O�@�&�@�%@�|�@��-@�o@�?}@���@���@��H@�p�@��@��h@�r�@�r�@�z�@��;@�n�@�@���@�X@���@��-@���@�@�@��^@��7@�p�@��h@���@��-@�x�@��@���@�Q�@�(�@��;@�@��+@�v�@�=q@��@��h@��@��@���@�@�$�@�-@�M�@�5?@���@��`@�%@���@�J@�E�@���@�^5@�;d@��P@��w@��+@��7@�`B@���@�A�@��@�Ĝ@���@�r�@�bN@�b@��@� �@�b@�  @�1'@�1'@��@��@��y@�ȴ@���@��!@�n�@���@�p�@��@��@��@�9X@�I�@��@���@��@�S�@�+@���@���@�ȴ@�ȴ@�ȴ@��@�hs@���@�Z@�r�@�I�@��w@�|�@�\)@�S�@�C�@�33@��H@�~�@�ff@�$�@���@��h@��@�z�@��@��
@��w@��
@��
@��;@��F@��@�9X@��w@��@��y@��@�?}@���@���@�j@�z�@��D@�bN@�1'@�b@�A�@��@��@�Z@�1'@�b@��m@��;@�Dg@~�x@s˒@k!-@d�@^GE@TA�@M;@DV�@>�@9@2:*@-8�@'��@#�4@n�@r�@'R@��@�$@	�Z111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A�A�  A�  A�  A�A�A�A�%A�
=A�1A�
=A�
=A�JA�JA�1A�A���A���A��A��A��A���A���A��A��A��A��A��A��A��A��A���A�(�A�7LAԴ9A�jA�dZA�\)A��A�A���A�XA��HA�\)AɬA�|�A�M�A���A�E�AǇ+A��A�5?A��A�1A¶FA�A��A�n�A��FA�33A���A�{A��A�K�A�M�A���A�%A��9A�7LA���A��^A�A��A��A� �A��uA�^5A��A�n�A��7A�%A��RA�t�A�l�A���A�E�A�dZA�~�A��A�$�A��A��A�S�A�33A�ƨA�\)A�&�A���A�7LA�;dA���A�Q�A��^A�S�A�ffA�C�A��TA�VA�S�A���A��FA��A��A�x�A�5?A��A��
A�|�A��-A���A�p�A�jA�/A���A�-A�r�A���A~~�Ax�9Aut�As�-Ap��AmdZAlA�Aj�Ahn�Ag��Ag��Ag�Ae�Acl�AahsA_A^r�A]��A]7LA\1AY��AWdZAU33AS��ARn�AP��AM;dAJAH�uAD��A?�FA:��A7��A69XA5�
A5��A5x�A5dZA5K�A5%A4M�A3�PA2ĜA21A0�9A//A.�HA.��A-�^A+�A)�A(��A(bA%�-A$�A"��A �/A �uA n�A 5?A�FA�DA&�A��An�A^5AM�A�A�A9XAx�A=qA?}AZA�TA��AoA^5A�Av�A�A�A��A{A��A�wA�A
��A	`BA��AM�A�A�wA�PAG�A�`A^5A�^A"�A ��A ��A �D@�ƨ@��\@�ff@���@��/@��@�Z@�1@�K�@���@�  @��m@�S�@�/@��@�V@�@�7@��@�@�@�o@柾@��#@�%@�$�@ى7@�+@�E�@�x�@�b@�S�@�ȴ@�n�@�x�@���@���@�t�@���@ΰ!@�^5@͑h@���@�9X@ʏ\@ɡ�@�O�@�V@�9X@ǥ�@�+@�V@őh@�?}@���@ļj@ě�@Õ�@+@�M�@�@�@�@�@���@���@�I�@�Z@�dZ@�+@�ȴ@���@���@��9@�1@�C�@��y@�`B@���@�%@�j@�1'@�  @�j@�+@��@��@�33@�@�33@�;d@�n�@�ff@�ff@��@�O�@��@��@�{@��7@���@�&�@���@��T@��-@�p�@�O�@�&�@�%@�|�@��-@�o@�?}@���@���@��H@�p�@��@��h@�r�@�r�@�z�@��;@�n�@�@���@�X@���@��-@���@�@�@��^@��7@�p�@��h@���@��-@�x�@��@���@�Q�@�(�@��;@�@��+@�v�@�=q@��@��h@��@��@���@�@�$�@�-@�M�@�5?@���@��`@�%@���@�J@�E�@���@�^5@�;d@��P@��w@��+@��7@�`B@���@�A�@��@�Ĝ@���@�r�@�bN@�b@��@� �@�b@�  @�1'@�1'@��@��@��y@�ȴ@���@��!@�n�@���@�p�@��@��@��@�9X@�I�@��@���@��@�S�@�+@���@���@�ȴ@�ȴ@�ȴ@��@�hs@���@�Z@�r�@�I�@��w@�|�@�\)@�S�@�C�@�33@��H@�~�@�ff@�$�@���@��h@��@�z�@��@��
@��w@��
@��
@��;@��F@��@�9X@��w@��@��y@��@�?}@���@���@�j@�z�@��D@�bN@�1'@�b@�A�@��@��@�Z@�1'@�b@��mG�O�@�Dg@~�x@s˒@k!-@d�@^GE@TA�@M;@DV�@>�@9@2:*@-8�@'��@#�4@n�@r�@'R@��@�$@	�Z111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	ƨB	ǮB	ƨB	ƨB	ƨB	ƨB	ƨB	ƨB	ƨB	ƨB	ƨB	ǮB	ǮB	ǮB	ǮB	ȴB	ȴB	ȴB	ɺB	ɺB	ɺB	ɺB	ɺB	ɺB	ɺB	ɺB	ɺB	ɺB	ɺB	ɺB	ɺB	ǮB	��B	�FB	��B	�-B	�#B
�B
��B
�}B
��B.BD�BD�BB�BF�BJ�BW
Bk�Bu�By�B��B�BƨB�)B�5B��B��B��B��B��B��B�DB�=B�PB�JBy�B��B��B�B�5B�yB�sB�mB�B��B��B�B�mB�B"�B%�B)�B49BE�BF�BG�BI�BT�BXBffB^5BYBS�BQ�BP�BO�BN�BJ�B:^B1'B)�B�B+B�B�BB�BB�B��Bs�BgmBYBJ�B�B\B
�B
�;B
��B
ĜB
�qB
�jB
�LB
��B
�7B
m�B
ZB
B�B
�B	��B	�NB	��B	�9B	��B	��B	�bB	�7B	�%B	�B	y�B	m�B	dZB	YB	M�B	J�B	E�B	>wB	5?B	&�B	�B	uB	DB	B�B�/B��B��B�B�{B�B�B� B� B� B~�B~�B~�B~�B�B�%B�1B�DB�=B�=B�7B�7B�=B�=B�7B�+B�%B�B|�Bs�Bq�Bp�Bo�Bn�Bn�Bp�Bp�Bs�Bt�Bv�B}�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B� B~�Bz�Bv�Bs�Bl�BbNBS�BS�BR�BR�BR�BS�BYB^5BcTBiyBk�Bm�Bo�Bu�B{�B�B�JB�\B��B��B��B��B��B��B��B�hB�1B�B|�Bw�Bt�Bp�Bo�Bo�Bp�Bo�Bn�BffBB�B6FB33B5?B6FB7LB9XB9XB<jB=qB=qB=qB>wB>wB?}B@�B@�BA�BF�BG�BH�BI�BN�BQ�BS�BVB^5BdZBhsBq�Br�Bw�Bw�B|�B�+B�PB�hB��B��B��B��B��B��B��B��B��B��B��B��B��B�!B�-B�RB�^B�dB�wB�}B�XB�9B�dB��BƨBȴB��B��B��B��B��B�
B�B�B��B��B��B��B�B�NB�sB�B�B�B�B�B��B��B�B�B�B�B�B�`B�sB�B��B��B��B��B�B�B��B��B	B	%B	1B	JB	bB	oB	�B	�B	�B	!�B	&�B	)�B	-B	33B	49B	5?B	6FB	6FB	6FB	8RB	;dB	=qB	=qB	:^B	5?B	6FB	;dB	>wB	@�B	A�B	B�B	F�B	I�B	K�B	Q�B	VB	ZB	`BB	aHB	hsB	jB	m�B	iyB	ffB	dZB	dZB	dZB	gmB	jB	k�B	k�B	k�B	l�B	n�B	n�B	q�B	t�B	w�B	x�B	{�B	}�B	� B	�B	�B	�B	�+B	�7B	�=B	�PB	�\B	�\B	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�3B	�FB	�FB	�RB	�XB	�^B	�jB	�jB	�jB	�qB	��B	��B	ÖB	ĜB	ƨB	ɺB	��B	��B	��B	��B	��B	ɺB	ȴB	ȴB	ȴB	��B	��B	��B	��B	��B	�
B	�#B	�)B	�5B	�;B	�BB	�BB	�BB	�B	��B	��B
�B
vB
�B
(�B
/OB
9�B
>�B
E�B
L�B
O�B
U�B
[�B
_B
f2B
jeB
p�B
u�B
xR111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B	�8B	�>B	�8B	�8B	�8B	�8B	�8B	�8B	�8B	�8B	�8B	�>B	�>B	�>B	�>B	�DB	�DB	�DB	�JB	�JB	�JB	�JB	�JB	�JB	�JB	�JB	�JB	�JB	�JB	�JB	�JB	�>B	�B	��B	�B	��B	ѴB
-B
�ZB
��B
�qB$�B;B;B9B= BA8BM�Ba�Bl8BpOB�$B��B�BҗBԣB�VB�1B�B�EB�KB��B��B��B��B��BpRB�B�9B�uBԦB��B��B��B�B�3B�PB�B��BB>BPB iB*�B<B=B>B@%BKiBN{B\�BT�BO�BJdBHXBGQBFKBEEBA.B0�B'�B lB#B��B�*BָB�zB�B��B�9Bj5B]�BO�BACB>B�B
�9B
��B
�ZB
�*B
��B
��B
��B
�yB
�B
d&B
P�B
9'B
6B	�[B	��B	ȎB	��B	��B	�kB	�
B	�B	|�B	{�B	p�B	d<B	[B	O�B	D�B	AoB	<PB	5&B	+�B	�B	^B	
(B	�B��B�TB��BʱB�CB��B�:B{�Bx�Bv�Bv�Bv�Bu�Bu�Bu�Bu�By�B|�B~�B�B��B��B�B�B��B��B�B}�B|�Bw�Bs�BjzBhoBgiBfcBe]Be]BgiBgiBj{Bk�Bm�Bt�By�Bz�By�Bz�By�Bz�By�By�By�By�By�By�Bx�Bx�Bx�Bv�Bu�Bq�Bm�Bj}BcSBYBJ�BJ�BI�BI�BI�BJ�BO�BT�BZB`BBbNBdZBfgBl�Br�B{�B�B�#B�HB�ZB�aB�rB�gB�aB�TB�0B~�Bx�Bs�Bn�Bk�BgoBfiBfiBgoBfiBecB]2B9^B-B*B,B-B.B0(B0(B3:B4AB4AB4AB5GB5GB6MB7SB7SB8YB=xB>~B?�B@�BE�BH�BJ�BL�BUB[(B_ABhwBi}Bn�Bn�Bs�B}�B�B�3B�LB�LB�RB�LB��B�}B��B��B��B��B��B��B��B��B��B�B�'B�-B�@B�FB�!B�B�-B�RB�qB�|BěBǭBɺB��B��B��B��B��BƧB��B��B��B��B�B�:B�QB�jB�pB�vB�pB�B�B�pB�LB�LB�qB�FB�(B�;B�qB�B�B�B�B�wB�qB�B�B��B��B��B	B	'B		4B	FB	WB	dB	�B	�B	 �B	#�B	)�B	*�B	,B	-B	-	B	-	B	/B	2&B	43B	43B	1 B	,B	-	B	2'B	59B	7EB	8KB	9QB	=jB	@|B	B�B	H�B	L�B	P�B	WB	XB	_2B	a>B	dPB	`8B	]&B	[B	[B	[B	^-B	a>B	bDB	bDB	bDB	cJB	eWB	eWB	hiB	k{B	n�B	o�B	r�B	t�B	v�B	w�B	w�B	{�B	}�B	�B	��B	�B	�B	�B	�%B	�2B	�DB	�JB	�JB	�JB	�OB	�\B	�hB	�nB	�tB	�tB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�$B	�$B	�%B	�+B	�=B	�CB	�PB	�VB	�bB	�tB	ÆB	ÆB	B	B	B	�tB	�nB	�nB	�nB	B	ŒB	ɫB	ɫB	ɫB	��B	��B	��B	��B	��B	��B	��G�O�B	ٽB	�B	�QB	�yB
,B
�B
vB
&B
0\B
5�B
<<B
CLB
FyB
L5B
RsB
U�B
\�B
aB
g�B
luB
o111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.09 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9998(+/-0.0001), vertically averaged dS =-0.009(+/-0.004) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144212022020411442120220204114421  AO  ARCAADJP                                                                    20200619170917    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200619170917  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200619170917  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114421  IP                  G�O�G�O�G�O�                