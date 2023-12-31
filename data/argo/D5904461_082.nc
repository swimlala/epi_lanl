CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-11-08T20:17:42Z AOML 3.0 creation; 2016-08-07T21:36:40Z UW 3.1 conversion     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7    PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7X   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8`   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~            8l   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8t   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8|   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
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
resolution        :�o     �  U|   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]p   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _p   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    gd   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  id   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qX   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yL   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {L   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �@   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �@   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �4   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �d   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �d   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �d   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �d   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20151108201742  20160807143640  5904461 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               RA   AO  5286_8897_082                   2C  D   APEX                            6531                            072314                          846 @�}�
�41   @�} ffv�@2�7Kƨ�c�
=p�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    RA   B   B   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B��B   B'��B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B���B���B���B�  B���B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  DtS3Dy�3D�3D�<�D�s3D�� D�fD�0 D�p D�ٚD�fD�<�D�� D��3D��D�I�Dڌ�D�ٚD���D�P D�fD��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��A�\A>�\A^�\A~�\A�G�A�G�A�G�A�G�A�G�A�G�A�G�A�G�B��B��B=qB��B'=qB/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B���B���B���B���B���B�k�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C�HC�HC�HC��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{D z=D �=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=D	z=D	�=D
z=D
�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=D z=D �=D!z=D!�=D"z=D"�=D#z=D#�=D$z=D$�=D%z=D%�=D&z=D&�=D'z=D'�=D(z=D(�=D)z=D)�=D*z=D*�=D+z=D+�=D,z=D,�=D-z=D-�=D.z=D.�=D/z=D/�=D0z=D0�=D1z=D1�=D2z=D2�=D3z=D3�=D4z=D4�=D5z=D5�=D6z=D6�=D7z=D7�=D8z=D8�=D9z=D9�=D:z=D:�=D;z=D;�=D<z=D<�=D=z=D=�=D>z=D>�=D?z=D?�=D@z=D@�=DAz=DA�=DBz=DB�=DCz=DC�=DDz=DD�=DEz=DE�=DFz=DF�=DGz=DG�=DHz=DH�=DIz=DI�=DJz=DJ�=DKz=DK�=DLz=DL�=DMz=DM�=DNz=DN�=DOz=DO�=DPz=DP�=DQz=DQ�=DRz=DR�=DSz=DS�=DTz=DT�=DUz=DU�=DVz=DV�=DWz=DW�=DXz=DX�=DYz=DY�=DZz=DZ�=D[z=D[�=D\z=D\�=D]z=D]�=D^z=D^�=D_z=D_�=D`z=D`�=Daz=Da�=Dbz=Db�=Dcz=Dc�=Ddz=Dd�=Dez=De�=Dfz=Df�=Dgz=Dg�=Dhz=Dh�=Diz=Di�=Djz=Dj�=Dkz=Dk�=Dlz=Dl�=Dmz=Dm�=Dnz=Dn�=Doz=Do�=Dpz=Dp�=Dqz=Dq�=Drz=Dr�=Dsz=Ds�=DtMpDy�pD�RD�9�D�pRD��D��D�-D�mD�ֹD��D�9�D��D��RD�	�D�F�Dډ�D�ֹD���D�MD�D��R11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�dZA�bNA�bNA�bNA�`BA�ffA�n�A�hsA�l�A�r�A�t�A�t�A�v�A�v�A�x�A�z�A�x�A�z�A�z�A�x�A�x�A�z�A�x�AڃAڅAڅAڇ+AڋDAڏ\AڍPAڏ\Aڏ\Aڝ�Aڰ!A�{A��;Aۉ7A�ffAٛ�A�dZA�A�A�ȴA�|�A�XA�-A�$�A��Aĥ�A�A�A�A�A��A�Q�A�S�A��;A�t�A���A�/A���A�%A�A�^5A�"�A�`BA���A���A��!A�ffA�S�A��A�^5A�JA���A���A�|�A�Q�A�n�A���A��RA���A�p�A��uA��yA�bNA��A��`A���A�bA��A��FA��
A���A�x�A��yA��A�oA���A�+A�`BA��/A�JA��hA��PA��hA���A�+A��TAdZA};dAx�uAvM�As�^Ap��AlA�AjI�Af��AdI�A`�DA[��AZ1'AW�FAV(�AS�;AQ��AQC�ANz�AL$�AK��AJ�+AH��AFbNACG�AB-AA�A?A=;dA;��A9A8��A8r�A7�FA6-A3G�A1�7A/��A.��A-&�A+�7A*~�A)p�A(��A(r�A&��A%�A$�9A$M�A"�A =qAK�AȴA��A33AJAjAA�Ap�A�A�RA=qAt�AjA �Ap�A7LA�HA�DA�A��AK�A�A�DAJAƨA��AO�AbA��A
��A�TA��A��A\)Av�A��A��A��A�DA E�@��^@��F@�ƨA ZA��A�
A�A/AK�A�RA �A�A 1'@�/@��#@�9X@�;d@��y@�+@���@�bN@�{@�@���@��@ꟾ@ꟾ@�Ĝ@�o@��@�ff@�S�@�+@�X@�`B@�V@ߕ�@�9X@ە�@�9X@�@ԃ@�1'@�E�@�7@�ȴ@�z�@��y@�C�@؃@�33@Դ9@ѩ�@ϕ�@�|�@�+@���@�?}@̴9@�ƨ@���@ɑh@�9X@��y@Ə\@�;d@�O�@�G�@���@��#@���@���@��@���@�ff@�33@���@�E�@�$�@���@���@�z�@�I�@��@�  @��@��+@��@�bN@��@�ƨ@��F@��P@���@��/@�7L@�V@�b@��@�V@��^@�Z@�v�@�z�@�9X@��@�dZ@�x�@���@��@��P@��P@�dZ@�"�@�+@�S�@���@��F@�I�@���@���@��9@�Z@�Ĝ@��h@��@�\)@��F@�^5@���@���@��\@�n�@�ȴ@��!@�~�@�n�@���@��!@�v�@�M�@�=q@���@�G�@�?}@�?}@�7L@�7L@��u@�l�@��@���@��H@�^5@�hs@���@��m@�"�@��!@�V@�-@�@�z�@��m@�|�@���@��/@��j@���@���@�@��y@��\@�V@��y@���@�&�@��`@�bN@�j@��D@�Z@���@��
@�Z@�z�@�S�@�M�@�^5@�=q@�@��-@��@��u@��j@���@���@��u@�Q�@�ƨ@�|�@�E�@�-@�J@���@�?}@��/@�Ĝ@�z�@��;@�"�@��!@�&�@�j@�r�@�"�@�v�@��@�=q@���@�I�@��u@��9@���@���@�`B@�x�@�p�@�x�@��h@��T@��@���@��@��@���@��h@�/@��@�%@��/@�9X@��F@�t�@�dZ@�;d@�K�@�C�@�@���@�=q@���@�7L@�%@�7L@�%@�j@�Q�@�1@�S�@��+@�ff@�E�@�5?@�$�@�M�@���@���@��\@�n�@�5?@��T@�hs@�V@�%@���@�Q�@�b@��@�S�@�33@���@���@�M�@�-@��@��h@�x�@�X@�?}@��@~�R@q�7@jn�@_K�@V�y@P�`@K�F@B~�@<��@3o@.@(�u@#C�@�;@�@�+@n�@\)@��@1'@5?11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A�dZA�bNA�bNA�bNA�`BA�ffA�n�A�hsA�l�A�r�A�t�A�t�A�v�A�v�A�x�A�z�A�x�A�z�A�z�A�x�A�x�A�z�A�x�AڃAڅAڅAڇ+AڋDAڏ\AڍPAڏ\Aڏ\Aڝ�Aڰ!A�{A��;Aۉ7A�ffAٛ�A�dZA�A�A�ȴA�|�A�XA�-A�$�A��Aĥ�A�A�A�A�A��A�Q�A�S�A��;A�t�A���A�/A���A�%A�A�^5A�"�A�`BA���A���A��!A�ffA�S�A��A�^5A�JA���A���A�|�A�Q�A�n�A���A��RA���A�p�A��uA��yA�bNA��A��`A���A�bA��A��FA��
A���A�x�A��yA��A�oA���A�+A�`BA��/A�JA��hA��PA��hA���A�+A��TAdZA};dAx�uAvM�As�^Ap��AlA�AjI�Af��AdI�A`�DA[��AZ1'AW�FAV(�AS�;AQ��AQC�ANz�AL$�AK��AJ�+AH��AFbNACG�AB-AA�A?A=;dA;��A9A8��A8r�A7�FA6-A3G�A1�7A/��A.��A-&�A+�7A*~�A)p�A(��A(r�A&��A%�A$�9A$M�A"�A =qAK�AȴA��A33AJAjAA�Ap�A�A�RA=qAt�AjA �Ap�A7LA�HA�DA�A��AK�A�A�DAJAƨA��AO�AbA��A
��A�TA��A��A\)Av�A��A��A��A�DA E�@��^@��F@�ƨA ZA��A�
A�A/AK�A�RA �A�A 1'@�/@��#@�9X@�;d@��y@�+@���@�bN@�{@�@���@��@ꟾ@ꟾ@�Ĝ@�o@��@�ff@�S�@�+@�X@�`B@�V@ߕ�@�9X@ە�@�9X@�@ԃ@�1'@�E�@�7@�ȴ@�z�@��y@�C�@؃@�33@Դ9@ѩ�@ϕ�@�|�@�+@���@�?}@̴9@�ƨ@���@ɑh@�9X@��y@Ə\@�;d@�O�@�G�@���@��#@���@���@��@���@�ff@�33@���@�E�@�$�@���@���@�z�@�I�@��@�  @��@��+@��@�bN@��@�ƨ@��F@��P@���@��/@�7L@�V@�b@��@�V@��^@�Z@�v�@�z�@�9X@��@�dZ@�x�@���@��@��P@��P@�dZ@�"�@�+@�S�@���@��F@�I�@���@���@��9@�Z@�Ĝ@��h@��@�\)@��F@�^5@���@���@��\@�n�@�ȴ@��!@�~�@�n�@���@��!@�v�@�M�@�=q@���@�G�@�?}@�?}@�7L@�7L@��u@�l�@��@���@��H@�^5@�hs@���@��m@�"�@��!@�V@�-@�@�z�@��m@�|�@���@��/@��j@���@���@�@��y@��\@�V@��y@���@�&�@��`@�bN@�j@��D@�Z@���@��
@�Z@�z�@�S�@�M�@�^5@�=q@�@��-@��@��u@��j@���@���@��u@�Q�@�ƨ@�|�@�E�@�-@�J@���@�?}@��/@�Ĝ@�z�@��;@�"�@��!@�&�@�j@�r�@�"�@�v�@��@�=q@���@�I�@��u@��9@���@���@�`B@�x�@�p�@�x�@��h@��T@��@���@��@��@���@��h@�/@��@�%@��/@�9X@��F@�t�@�dZ@�;d@�K�@�C�@�@���@�=q@���@�7L@�%@�7L@�%@�j@�Q�@�1@�S�@��+@�ff@�E�@�5?@�$�@�M�@���@���@��\@�n�@�5?@��T@�hs@�V@�%@���@�Q�@�b@��@�S�@�33@���@���@�M�@�-@��@��h@�x�@�X@�?}G�O�@~�R@q�7@jn�@_K�@V�y@P�`@K�F@B~�@<��@3o@.@(�u@#C�@�;@�@�+@n�@\)@��@1'@5?11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBBBBBBBBBBBBBBBBBBBBBBBB1B+B+B1B	7BDB	7B	7B	7B\B�BE�B�}B��B�BJB'�B6FBe`B��B�B��B��B�;B�`B�B�B��BB	7B1B+B%BBBBBB+BB%BB��B��B�NB��BÖB�-B��B��B��B��B��B�hB�\B�VB�7B{�BdZB\)BW
BP�B;dB#�BhB  B�B�#BŢB�XB�B�BgmB:^B�B
��B
�B
�FB
�B
_;B
I�B
2-B
�B
B	�B	��B	�^B	��B	�=B	hsB	XB	@�B	0!B	�B	%B��B�B�B�NB�)B�
B��B��B�wB�XB�'B��B��B��B��B��B��B��B��B��B��B��B�\B�=B�=B�VB�oB��B��B��B��B��B�{B�bB�bB�VB�=B~�Bz�B|�Bx�Bp�Bm�BffBn�BbNB_;B_;B_;B]/BZBXB]/BZB^5B�PB��B��B��B�qB�qB�qB�qB�}B��BÖBÖB�wB�uBw�Bk�Bl�By�B�\B�=B�Bm�BYBI�B@�BN�BVBl�B��B��B��B��B�B�`B�ZB��B�FB� B[#BQ�BM�BP�BP�BP�BVBl�Bx�Bm�Bm�B��B��B�\B|�B{�B��B�B�B�XB��BȴB�qB�B�B�3B��B��B�'B�B��B�B��B��B�B�B�mB�;B�B��B��B�B�#B�B�B�B�
B��B��B�
B�;B�mB�HB�
B��B��B��B�5B�BɺB��B�RB�LB�?B�?B�9B�FB�XB�jB�wB�wB��BƨB��B�B�B�5B�NB�yB�B��B��B	  B	  B	
=B	{B	�B	�B	hB	�B	�B	�B	uB	\B	�B	�B	�B	�B	�B	�B	$�B	(�B	.B	1'B	7LB	A�B	C�B	D�B	E�B	K�B	S�B	XB	bNB	}�B	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�oB	�\B	�JB	�=B	�+B	�B	�B	�B	�B	�B	�B	�B	�B	�7B	�{B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	�B	�3B	�?B	�9B	�?B	�^B	�qB	�jB	�dB	�dB	�jB	�jB	�qB	�jB	�qB	��B	ÖB	ŢB	ƨB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�#B	�/B	�;B	�BB	�NB	�TB	�ZB	�fB	�mB	�sB	�sB	�sB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
B
B
B
%B
%B
%B
1B
	7B
�B
 �B
(�B
2-B
8RB
<jB
D�B
H�B
O�B
T�B
[#B
^5B
ffB
iyB
m�B
q�B
t�B
x�B
{�B
}�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B#B#B#B#B#B"B#B"B"B"B!B#B"B"B"B#B"BB'B#B#B"B B=B5B5B9B	CBNB	CB	BB	ABbB�BE�B��B�B�BQB'�B6MBeiB��B� B��B��B�CB�jB�B��B��BB	>B<B5B.B'BBBB)B8B*B0B"B��B��B�WB��BÞB�6B�B��B��B��B��B�pB�gB�^B�=B{�Bd_B\0BWBP�B;jB#�BsB B�B�-BŨB�aB�#B�*Bg{B:iB�B
��B
�B
�SB
�B
_IB
I�B
2@B
�B
B	��B	��B	�pB	��B	�TB	h�B	X&B	@�B	0:B	�B	>B�B��B�B�kB�FB�(B��B��B��B�uB�DB�B��B��B��B��B��B��B��B��B��B��B�|B�\B�^B�vB��B��B��B��B��B��B��B��B��B�uB�]BB{ B}Bx�Bp�Bm�Bf�Bn�BboB_[B_]B_^B]SBZ@BX4B]RBZ>B^WB�oB��B��B�B��B��B��B��B��B��BòBóB��B��Bw�Bk�Bl�By�B�{B�]B�2Bm�BY8BI�B@�BN�BV&Bl�B��B��B�B��B�:B�{B�xB��B�dB�#B[FBRBM�BQBQBQBV'Bl�Bx�Bm�Bm�B�B�B�}B}B|B��B�,B�;B�sB�B��B��B�4B�8B�QB��B��B�DB�.B��B��B�B�B��B�B�B�VB�"B�B�B�;B�>B�4B�9B�,B�%B�B�B�&B�UB�B�bB�'B�	B��B�B�PB�/B��B��B�oB�jB�^B�]B�XB�cB�uB��B��B��B��B��B�B�+B�;B�QB�kB�B�B��B�B	 B	 B	
WB	�B	�B	�B	�B	�B	�B	�B	�B	vB	�B	�B	�B	�B	�B	�B	$�B	)B	.,B	1?B	7bB	A�B	C�B	D�B	E�B	K�B	TB	X$B	bfB	~B	�}B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�rB	�^B	�TB	�BB	�5B	�5B	�5B	�-B	�(B	�B	�B	�(B	�LB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�	B	�B	�)B	�IB	�TB	�LB	�OB	�tB	��B	�}B	�zB	�yB	�~B	��B	��B	�~B	��B	��B	êB	ŵB	ƼB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�*B	�5B	�BB	�PB	�VB	�_B	�eB	�mB	�zB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B
 B
B
B
B
B
%B
"B
%B
-B
1B
6B
5B
6B
5B
1B
1B
4B
7B
7B
8G�O�B
	HB
�B
 �B
)B
2=B
8dB
<zB
D�B
H�B
O�B
UB
[2B
^CB
fvB
i�B
m�B
q�B
t�B
x�B
{�B
~11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.09 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071436402016080714364020160807143640  AO  ARCAADJP                                                                    20151108201742    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20151108201742  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20151108201742  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807143640  IP                  G�O�G�O�G�O�                