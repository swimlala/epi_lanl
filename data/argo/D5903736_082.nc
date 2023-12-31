CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:05:49Z AOML 3.0 creation; 2016-05-31T19:14:38Z UW 3.1 conversion     
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
resolution        :�o     �  U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    gx   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ix   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qp   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {h   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �`   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �`   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �X   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �(   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �,   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �0   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20140721230549  20160531121438  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               RA   AO  4051_7090_082                   2C  D   APEX                            5368                            041511                          846 @�y�|@1   @�z���@2��^5?}�d�hr� �1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    RA   A   A   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(ffB0��B733B@  BH  BP  BX  B`  Bh  Bp  BxffB�33B�  B���B�  B�  B���B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC�fDD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�3Dy� D� D�L�D�p D���D�fD�33D��fD���D���D�C3D�y�D���D�	�D�,�D�l�D��3D�3D�9�D�p D��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�Q�@��@��A�\A>�\A^�\A~�\A�G�A�G�A�G�A�G�A�G�A�G�A�G�A�G�B��B��B��B��B(
=B0p�B6�
B?��BG��BO��BW��B_��Bg��Bo��Bx
=B�B���B���B���B���B���B���B���B�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C�HC��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{D z=D �=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=D	z=D	�=D
z=D
�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=D z=D �=D!z=D!�=D"z=D"�=D#z=D#�=D$z=D$�=D%z=D%�=D&z=D&�=D'z=D'�=D(z=D(�=D)z=D)�=D*z=D*�=D+z=D+�=D,z=D,�=D-z=D-�=D.z=D.�=D/z=D/�=D0z=D0�=D1z=D1�=D2z=D2�=D3z=D3�=D4z=D4�=D5z=D5�=D6z=D6�=D7z=D7�=D8z=D8�=D9z=D9�=D:z=D:�=D;z=D;�=D<z=D<�=D=z=D=�=D>z=D>�=D?z=D?�=D@z=D@�=DAz=DA�=DBz=DB�=DC��DC�=DDz=DD�=DEz=DE�=DFz=DF�=DGz=DG�=DHz=DH�=DIz=DI�=DJz=DJ�=DKz=DK�=DLz=DL�=DMz=DM�=DNz=DN�=DOz=DO�=DPz=DP�=DQz=DQ�=DRz=DR�=DSz=DS�=DTz=DT�=DUz=DU�=DVz=DV�=DWz=DW�=DXz=DX�=DYz=DY�=DZz=DZ�=D[z=D[�=D\z=D\�=D]z=D]�=D^z=D^�=D_z=D_�=D`z=D`�=Daz=Da�=Dbz=Db�=Dcz=Dc�=Ddz=Dd�=Dez=De�=Dfz=Df�=Dgz=Dg�=Dhz=Dh�=Diz=Di�=Djz=Dj�=Dkz=Dk�=Dlz=Dl�=Dmz=Dm�=Dnz=Dn�=Doz=Do�=Dpz=Dp�=Dqz=Dq�=Drz=Dr�=Dsz=Ds�=Dtz=Dt�pDy�=D�D�I�D�mD���D��D�0RD���D���D���D�@RD�v�D���D��D�)�D�i�D��RD�RD�6�D�mD�Å111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���AݼjAݺ^AݸRAݾwAݼjA�A���AݮAݩ�AݮAݮAݮAݮAݧ�Aݲ-Aݟ�Aݙ�Aݗ�A݅A�n�A��A�O�A�AՋDA��A��A�/A�v�Aϣ�A��A�x�Aͩ�A˩�A�1A���Aɧ�AǺ^A�K�A�p�Aġ�A�ƨA���A���A��A�hsA�  A�r�A���A��PA�5?A���A���A���A��7A�XA�~�A�ZA�A�A��!A�
=A��A��DA���A��^A�&�A���A�Q�A�VA�ĜA�l�A�v�A�|�A��7A���A��A���A��+A�r�A�|�A�1'A�  A���A���A�JA�v�A�JA�z�A� �A�p�A��wA��HA��A�1A�oA�p�A�9XA�oA�XA��;A�ZA�A�A�ffA�S�A���A�$�A��A��A��TA��A���A���A�A�A�%A��\A�33A���A�C�A�  A���A��#A��!A���A�1A~�A|ZAy;dAu��As7LAq��Ao+Am�TAm�AkC�Ai�AgƨAf�+Ae|�Ac�#Ab^5A_|�A\r�AZĜAX�AXAW�wAW�hAW`BAU�^ARbNAQ�-AP��AN��AL��AJ�HAI��AG/AD��AC"�AA��A@��A?�A<bA7C�A6^5A5/A5
=A4��A3hsA21A-ƨA+oA({A'��A&�9A$��A#/A�A��A+AbNA�mA\)A�!Az�AI�A�^Ax�AJA�A�mA|�AXAA�+A �Az�A��AƨA/AQ�A �A�A�AA
�`A	�^A	&�A�#AoAVA�wA�A�A^5AI�A��AffAp�AG�A/A ��A �u@���@�A�@�33@�^5@��h@�A�@���@�Z@�@��T@�@�@�h@�@�ȴ@�v�@���@�&�@��/@�r�@� �@�\)@���@�Q�@�t�@柾@�$�@�@��/@��@�Q�@��@��;@�;d@���@��@�  @�&�@�33@�v�@�M�@ڧ�@ڰ!@��#@�Ĝ@�|�@��@�%@�I�@��@�z�@�A�@Ӿw@�K�@Ӆ@�ƨ@Ӿw@�t�@�\)@�K�@�C�@���@�J@�p�@�Ĝ@��@�o@���@Η�@�ff@�$�@ͩ�@�p�@̋D@�K�@�@���@�ff@��@ȼj@�|�@Ə\@Ł@���@�bN@���@�
=@�&�@��;@���@��@���@�Ĝ@�bN@��
@�|�@�
=@���@��!@�v�@��7@�O�@���@�r�@��w@�K�@�o@���@���@�M�@��#@��h@�O�@�Ĝ@�r�@��9@���@���@�1@�|�@�o@���@�n�@�@��T@��-@�%@���@��@��D@�bN@���@��@�=q@�X@�7L@�/@���@��m@��@���@�ff@��@��7@�O�@��@���@���@��;@�;d@��H@��!@�ff@�{@��-@�O�@��@���@��@�b@��;@�ƨ@��@��@�|�@��@�C�@�
=@���@�ȴ@�ff@�$�@�@���@���@��@�O�@�%@��9@�r�@�Q�@��@���@��w@��@�;d@���@�E�@��T@�O�@�Ĝ@��@�Z@�1'@��;@��@�V@�-@�J@��T@���@�@��-@���@�hs@��j@�l�@�"�@���@��y@��y@��@���@�ȴ@�J@���@�V@��`@��@��u@�j@��@��P@�S�@�"�@��R@�o@�
=@��R@�^5@�5?@�$�@�{@���@�O�@�j@��m@�dZ@��@�^5@��@���@���@�p�@���@�r�@�A�@�  @��;@��
@��w@���@��P@�|�@�l�@�C�@��@��y@��\@�J@��j@��D@��D@��@�(�@���@�?}@�7L@z=q@r�H@h��@]�@T��@M/@G
=@?�w@8��@3ƨ@-�h@(�@!��@O�@��@`B@7L@/@	��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A���AݼjAݺ^AݸRAݾwAݼjA�A���AݮAݩ�AݮAݮAݮAݮAݧ�Aݲ-Aݟ�Aݙ�Aݗ�A݅A�n�A��A�O�A�AՋDA��A��A�/A�v�Aϣ�A��A�x�Aͩ�A˩�A�1A���Aɧ�AǺ^A�K�A�p�Aġ�A�ƨA���A���A��A�hsA�  A�r�A���A��PA�5?A���A���A���A��7A�XA�~�A�ZA�A�A��!A�
=A��A��DA���A��^A�&�A���A�Q�A�VA�ĜA�l�A�v�A�|�A��7A���A��A���A��+A�r�A�|�A�1'A�  A���A���A�JA�v�A�JA�z�A� �A�p�A��wA��HA��A�1A�oA�p�A�9XA�oA�XA��;A�ZA�A�A�ffA�S�A���A�$�A��A��A��TA��A���A���A�A�A�%A��\A�33A���A�C�A�  A���A��#A��!A���A�1A~�A|ZAy;dAu��As7LAq��Ao+Am�TAm�AkC�Ai�AgƨAf�+Ae|�Ac�#Ab^5A_|�A\r�AZĜAX�AXAW�wAW�hAW`BAU�^ARbNAQ�-AP��AN��AL��AJ�HAI��AG/AD��AC"�AA��A@��A?�A<bA7C�A6^5A5/A5
=A4��A3hsA21A-ƨA+oA({A'��A&�9A$��A#/A�A��A+AbNA�mA\)A�!Az�AI�A�^Ax�AJA�A�mA|�AXAA�+A �Az�A��AƨA/AQ�A �A�A�AA
�`A	�^A	&�A�#AoAVA�wA�A�A^5AI�A��AffAp�AG�A/A ��A �u@���@�A�@�33@�^5@��h@�A�@���@�Z@�@��T@�@�@�h@�@�ȴ@�v�@���@�&�@��/@�r�@� �@�\)@���@�Q�@�t�@柾@�$�@�@��/@��@�Q�@��@��;@�;d@���@��@�  @�&�@�33@�v�@�M�@ڧ�@ڰ!@��#@�Ĝ@�|�@��@�%@�I�@��@�z�@�A�@Ӿw@�K�@Ӆ@�ƨ@Ӿw@�t�@�\)@�K�@�C�@���@�J@�p�@�Ĝ@��@�o@���@Η�@�ff@�$�@ͩ�@�p�@̋D@�K�@�@���@�ff@��@ȼj@�|�@Ə\@Ł@���@�bN@���@�
=@�&�@��;@���@��@���@�Ĝ@�bN@��
@�|�@�
=@���@��!@�v�@��7@�O�@���@�r�@��w@�K�@�o@���@���@�M�@��#@��h@�O�@�Ĝ@�r�@��9@���@���@�1@�|�@�o@���@�n�@�@��T@��-@�%@���@��@��D@�bN@���@��@�=q@�X@�7L@�/@���@��m@��@���@�ff@��@��7@�O�@��@���@���@��;@�;d@��H@��!@�ff@�{@��-@�O�@��@���@��@�b@��;@�ƨ@��@��@�|�@��@�C�@�
=@���@�ȴ@�ff@�$�@�@���@���@��@�O�@�%@��9@�r�@�Q�@��@���@��w@��@�;d@���@�E�@��T@�O�@�Ĝ@��@�Z@�1'@��;@��@�V@�-@�J@��T@���@�@��-@���@�hs@��j@�l�@�"�@���@��y@��y@��@���@�ȴ@�J@���@�V@��`@��@��u@�j@��@��P@�S�@�"�@��R@�o@�
=@��R@�^5@�5?@�$�@�{@���@�O�@�j@��m@�dZ@��@�^5@��@���@���@�p�@���@�r�@�A�@�  @��;@��
@��w@���@��P@�|�@�l�@�C�@��@��y@��\@�J@��j@��D@��D@��@�(�@���@�?}@�7L@z=q@r�H@h��@]�@T��@M/@G
=@?�w@8��@3ƨ@-�h@(�@!��@O�@��@`B@7L@/@	��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBM�BM�BL�BL�BL�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BN�BK�B[#Bs�Bv�Bx�Bz�B}�B�\B�hB�{B��B��B�/BBDBXB�JB��B�3B�^B�wB�-B��B��B�1B�B�B�bB��B��B�3B��B��B�{B�JBs�B]/BL�BM�BQ�BS�BM�B:^B49B7LB5?B1'B&�B�B%B��B�B�ZB�)B�
B��BƨB�9B��B��B�B�B��B��B�oBm�BG�B$�B�B;dB7LB$�BoB  B�B�B�yB�B�dB�!B�B��B�+Bz�Bl�BM�B;dB%�B�BB
�B
�sB
�`B
�/B
�
B
��B
ȴB
��B
�RB
��B
�B
dZB
O�B
A�B
)�B
hB
B	��B	��B	�NB	��B	�/B	�;B	�B	ɺB	�^B	�B	��B	�\B	�B	r�B	n�B	o�B	q�B	p�B	o�B	l�B	bNB	R�B	N�B	G�B	=qB	33B	+B	"�B	�B	JB	B��B��B�B�HB��B��B��B��BȴBƨBBB��B��B�wB�wB�RB�?B�'B�B�B�B�'B�3B�?B�?B�9B�9B�'B�XB�XB�dB�jB�dB�dB�jB�jB��BBÖBÖBĜBÖBĜBÖB��B��BŢBǮB��B��B��B��B��B��B��B��B��B��B�B�
B�
B�
B�
B�#B�5B�/B�#B�)B�;B�HB�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B	B	B	DB	
=B	
=B	
=B		7B	DB	JB	JB	JB	DB	
=B	PB	PB	DB	
=B	JB	\B	uB	�B	�B	�B	uB	hB	uB	�B	$�B	.B	.B	-B	2-B	9XB	;dB	@�B	I�B	M�B	N�B	N�B	Q�B	T�B	T�B	VB	XB	XB	XB	W
B	W
B	W
B	VB	VB	T�B	S�B	S�B	S�B	R�B	Q�B	O�B	N�B	L�B	K�B	J�B	I�B	H�B	F�B	F�B	F�B	K�B	Q�B	S�B	S�B	W
B	YB	W
B	W
B	YB	\)B	^5B	_;B	aHB	cTB	dZB	dZB	e`B	ffB	ffB	gmB	hsB	jB	jB	k�B	n�B	s�B	|�B	� B	�B	�B	�+B	�1B	�1B	�7B	�JB	�PB	�PB	�bB	�hB	�hB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�-B	�3B	�9B	�RB	�^B	�dB	�jB	�wB	��B	��B	B	ÖB	ÖB	B	ĜB	ŢB	ŢB	ƨB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�B	�B	�B	�)B	�/B	�5B	�5B	�;B	�;B	�;B	�;B	�BB	�NB	�NB	�NB	�NB	�NB	�NB	�NB	�NB	�HB	�NB	�ZB	�`B	�mB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
PB
�B
!�B
(�B
33B
9XB
C�B
G�B
M�B
Q�B
W
B
[#B
aHB
gmB
k�B
n�B
s�B
w�B
z�B
� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  BM�BM�BL�BL�BL�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BM�BN�BK�B[,Bs�Bv�Bx�Bz�B}�B�dB�mB��B��B��B�7BBOBXB�TB��B�BB�kB��B�>B��B��B�?B�$B�B�pB��B��B�AB�B��B��B�YBs�B]=BL�BM�BQ�BTBM�B:gB4AB7XB5GB14B&�B�B2B��B�B�cB�5B�B��BƱB�BB��B��B�B�B�B��B�uBm�BG�B$�B�B;kB7VB$�BwB B�B�B�B�B�kB�+B�B��B�4Bz�Bl�BM�B;qB%�B�BB
�B
�~B
�lB
�<B
�B
��B
��B
��B
�_B
��B
�B
dgB
O�B
A�B
*	B
zB
B	�B	��B	�^B	�B	�AB	�NB	�B	��B	�rB	�#B	��B	�rB	�B	r�B	n�B	o�B	q�B	p�B	o�B	l�B	bdB	S	B	N�B	G�B	=�B	3KB	+B	"�B	�B	cB	4B��B��B�B�cB��B��B��B��B��B��B¯B­B��B��B��B��B�pB�]B�DB�;B�1B�2B�EB�PB�`B�`B�XB�YB�FB�tB�tB��B��B��B��B��B��B��B®BóBõBĻBõBĻBõB��B��B��B��B��B�B� B��B��B��B��B��B��B�
B�%B�'B�(B�'B�'B�?B�PB�LB�?B�HB�VB�bB�B��B�B�B�B�B��B��B��B��B��B��B��B��B�B	"B	9B	]B	
WB	
WB	
WB		SB	^B	bB	bB	cB	]B	
WB	jB	lB	^B	
UB	cB	wB	�B	�B	�B	�B	�B	�B	�B	�B	$�B	.-B	..B	-(B	2GB	9oB	;B	@�B	I�B	M�B	N�B	N�B	RB	UB	UB	VB	X'B	X'B	X%B	W!B	W"B	W B	VB	VB	UB	TB	TB	TB	SB	RB	O�B	N�B	L�B	K�B	J�B	I�B	H�B	F�B	F�B	F�B	K�B	RB	TB	TB	W!B	Y0B	W B	W!B	Y0B	\BB	^MB	_QB	a`B	clB	doB	dqB	exB	f|B	f~B	g�B	h�B	j�B	j�B	k�B	n�B	s�B	}B	�B	�)B	�4B	�@B	�GB	�IB	�NB	�bB	�fB	�eB	�{B	�}B	�~B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�(B	�/B	�6B	�AB	�EB	�NB	�gB	�pB	�wB	�~B	��B	��B	��B	£B	êB	êB	¤B	ĲB	ŷB	ŸB	ƼB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�#B	�"B	�"B	�$B	�+B	�*B	�/B	�;B	�AB	�JB	�KB	�LB	�OB	�NB	�MB	�WB	�aB	�aB	�`B	�`B	�aB	�bB	�aB	�aB	�\B	�`B	�mB	�rB	�B	�B	�B	�B	�B	��B	�B	�B	�B	�B	��B	��B	�B	�B	�B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	��B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B
*B
aB
�B
!�B
)
B
3FB
9iB
C�B
G�B
M�B
Q�B
WB
[2B
aXB
g~B
k�B
n�B
s�B
w�B
z�B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.09 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311214382016053112143820160531121438  AO  ARCAADJP                                                                    20140721230549    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230549  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230549  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531121438  IP                  G�O�G�O�G�O�                