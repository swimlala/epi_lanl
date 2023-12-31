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
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20140721230549  20160531121438  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               SA   AO  4051_7090_083                   2C  D   APEX                            5368                            041511                          846 @�x���1   @�	��@2�$�/��d����F1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    SA   A   A   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C�C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1�fD2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�3Dy� D�fD�I�D��3D��3D�  D�9�D�s3D��fD�fD�@ D��3D�� D���D�6fDڌ�D��3D���D�9�D�vfD�ɚ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��R@��@��A�\A>�\A^�\A~�\A�G�A�G�A�G�A�G�A�G�A�G�A�G�A�G�B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���BB���B���B���B���C��C��C��C��C	��C��C��C��C��C��C�C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C�HC��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C�HC�HC��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C�HC��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{D z=D �=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=D	z=D	�=D
z=D
�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=D z=D �=D!z=D!�=D"z=D"�=D#z=D#�=D$z=D$�=D%z=D%�=D&z=D&�=D'z=D'�=D(z=D(�=D)z=D)�=D*z=D*�=D+z=D+�=D,z=D,�=D-z=D-�=D.z=D.�=D/z=D/�=D0z=D0�=D1��D1�=D2z=D2�=D3z=D3�=D4z=D4�=D5z=D5�=D6z=D6�=D7z=D7�=D8z=D8�=D9z=D9�=D:z=D:�=D;z=D;�=D<z=D<�=D=z=D=�=D>z=D>�=D?z=D?�=D@z=D@�=DAz=DA�=DBz=DB�=DCz=DC�=DDz=DD�=DEz=DE�=DFz=DF�=DGz=DG�=DHz=DH�=DIz=DI�=DJz=DJ�=DKz=DK�=DLz=DL�=DMz=DM�=DNz=DN�=DOz=DO�=DPz=DP�=DQz=DQ�=DRz=DR�=DSz=DS�=DTz=DT�=DUz=DU�=DVz=DV�=DWz=DW�=DXz=DX�=DYz=DY�=DZz=DZ�=D[z=D[�=D\z=D\�=D]z=D]�=D^z=D^�=D_z=D_�=D`z=D`�=Daz=Da�=Dbz=Db�=Dcz=Dc�=Ddz=Dd�=Dez=De�=Dfz=Df�=Dgz=Dg�=Dhz=Dh�=Diz=Di�=Djz=Dj�=Dkz=Dk�=Dlz=Dl�=Dmz=Dm�=Dnz=Dn�=Doz=Do�=Dpz=Dp�=Dqz=Dq�=Drz=Dr�=Dsz=Ds�=Dtz=Dt�pDy�=D��D�F�D��RD��RD��D�6�D�pRD�ӅD��D�=D��RD��D���D�3�Dډ�D��RD���D�6�D�s�D�ƹ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�+A�5?A�5?A�+A�&�A��A��A�
=A��A��`A��;A���AܸRAܝ�A�x�A�p�A�VA��A�S�A�oA��yAڅA�?}AمAغ^A׺^A��AӬA���A��HAЩ�A�=qA�x�A�Aȣ�A�1'A��Aũ�A���A�r�A���A���A��\A�`BA��uA�%A��A�A��uA��A�jA��A�?}A��/A�G�A�;dA���A�A�v�A���A���A���A�A�A�JA��A�E�A���A��mA�O�A���A���A��-A��!A���A�7LA��DA�&�A��A���A���A�?}A��-A�&�A�G�A���A�z�A��A���A���A��A�`BA��A��jA�XA���A��A�v�A�A�A�\)A��A�p�A���A�=qA��-A��A�v�A�ZA�1A���A�n�A� �A��uA���A�=qA}��A{\)Ax-At�Aq?}Ao��AmhsAj�Ah5?Ag
=Ad�yAd  Ab�AahsA_`BA[�7AZ�\AZ{AYVAW�-AU
=AS�AP��AO��AM��AKG�AI`BAG�#AF�AD�AC�hAA��A?/A=�mA=t�A<^5A9�A8�A6I�A5�
A5��A5��A5t�A5G�A3p�A2E�A1�A1�;A1��A1�7A0��A/��A,�`A+��A+�^A+\)A*{A(M�A'O�A&�!A%�#A$ȴA#\)A"(�A!�A M�AE�A��A��AhsA�FA�A�hA�A�AG�AjAZA�AVA�A�uA �A�AJA�AG�A$�A|�A
�A
��A
{A�A��A%A=qA��A�A�
AC�AVA1A ��A n�@���@�x�@�1@�"�@�ȴ@�v�@���@�Ĝ@�K�@�t�@��@�o@�M�@���@��m@�@���@��@�1'@�K�@�K�@�Z@�-@�9X@�^@��;@�j@�n�@���@�v�@���@���@���@�M�@���@��@�I�@�E�@���@�{@�Ĝ@�~�@٩�@�G�@؋D@�I�@� �@�K�@�O�@�l�@�5?@ӕ�@�G�@�hs@�/@�ƨ@ӍP@�o@�^5@�@�X@�bN@��T@��@��@�hs@�S�@˕�@�x�@��T@���@�hs@�Q�@̬@��@�&�@�(�@�
=@���@�33@ɑh@�r�@��@�b@Ǖ�@�-@��@���@Ų-@Ų-@��`@�Q�@�"�@�@�z�@��H@���@��h@�Q�@��h@�%@���@�ff@��!@�"�@�C�@�{@���@�&�@�7L@���@���@��+@���@�p�@��@�%@���@�Z@�"�@��@�\)@��m@��@��#@�?}@��@��j@���@���@� �@�ƨ@�l�@�n�@��`@�b@��u@���@�v�@���@��!@�E�@���@���@���@�b@�+@��;@��\@�p�@��@��y@��@� �@��@�@���@�n�@�E�@���@�`B@��/@���@�bN@�1'@��;@�t�@��@�ȴ@�~�@�=q@�{@��-@��@��@�p�@�X@���@��@��u@��@��@�z�@�j@�I�@��
@�|�@��@���@��R@���@���@�n�@�=q@�{@�@���@�O�@��`@�Ĝ@��u@�z�@�I�@�I�@�1@���@�dZ@��P@�S�@�33@��H@��+@��@���@���@�@�hs@��@���@�r�@� �@��@�33@�@�M�@�@�x�@�hs@�O�@�V@���@��D@�Z@�1'@� �@���@���@�K�@���@�@��^@���@���@�&�@��/@�r�@�1@���@�33@��R@��#@�Q�@��F@��P@�l�@��y@�n�@�M�@�{@��#@���@�`B@�%@��u@�r�@�Z@�1'@�  @��
@��w@�&�@�t�@}��@s"�@g��@\��@U�@J�@D(�@=�h@7+@/�w@*�\@%��@�@��@��@o@�+@�
@�9111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�+A�5?A�5?A�+A�&�A��A��A�
=A��A��`A��;A���AܸRAܝ�A�x�A�p�A�VA��A�S�A�oA��yAڅA�?}AمAغ^A׺^A��AӬA���A��HAЩ�A�=qA�x�A�Aȣ�A�1'A��Aũ�A���A�r�A���A���A��\A�`BA��uA�%A��A�A��uA��A�jA��A�?}A��/A�G�A�;dA���A�A�v�A���A���A���A�A�A�JA��A�E�A���A��mA�O�A���A���A��-A��!A���A�7LA��DA�&�A��A���A���A�?}A��-A�&�A�G�A���A�z�A��A���A���A��A�`BA��A��jA�XA���A��A�v�A�A�A�\)A��A�p�A���A�=qA��-A��A�v�A�ZA�1A���A�n�A� �A��uA���A�=qA}��A{\)Ax-At�Aq?}Ao��AmhsAj�Ah5?Ag
=Ad�yAd  Ab�AahsA_`BA[�7AZ�\AZ{AYVAW�-AU
=AS�AP��AO��AM��AKG�AI`BAG�#AF�AD�AC�hAA��A?/A=�mA=t�A<^5A9�A8�A6I�A5�
A5��A5��A5t�A5G�A3p�A2E�A1�A1�;A1��A1�7A0��A/��A,�`A+��A+�^A+\)A*{A(M�A'O�A&�!A%�#A$ȴA#\)A"(�A!�A M�AE�A��A��AhsA�FA�A�hA�A�AG�AjAZA�AVA�A�uA �A�AJA�AG�A$�A|�A
�A
��A
{A�A��A%A=qA��A�A�
AC�AVA1A ��A n�@���@�x�@�1@�"�@�ȴ@�v�@���@�Ĝ@�K�@�t�@��@�o@�M�@���@��m@�@���@��@�1'@�K�@�K�@�Z@�-@�9X@�^@��;@�j@�n�@���@�v�@���@���@���@�M�@���@��@�I�@�E�@���@�{@�Ĝ@�~�@٩�@�G�@؋D@�I�@� �@�K�@�O�@�l�@�5?@ӕ�@�G�@�hs@�/@�ƨ@ӍP@�o@�^5@�@�X@�bN@��T@��@��@�hs@�S�@˕�@�x�@��T@���@�hs@�Q�@̬@��@�&�@�(�@�
=@���@�33@ɑh@�r�@��@�b@Ǖ�@�-@��@���@Ų-@Ų-@��`@�Q�@�"�@�@�z�@��H@���@��h@�Q�@��h@�%@���@�ff@��!@�"�@�C�@�{@���@�&�@�7L@���@���@��+@���@�p�@��@�%@���@�Z@�"�@��@�\)@��m@��@��#@�?}@��@��j@���@���@� �@�ƨ@�l�@�n�@��`@�b@��u@���@�v�@���@��!@�E�@���@���@���@�b@�+@��;@��\@�p�@��@��y@��@� �@��@�@���@�n�@�E�@���@�`B@��/@���@�bN@�1'@��;@�t�@��@�ȴ@�~�@�=q@�{@��-@��@��@�p�@�X@���@��@��u@��@��@�z�@�j@�I�@��
@�|�@��@���@��R@���@���@�n�@�=q@�{@�@���@�O�@��`@�Ĝ@��u@�z�@�I�@�I�@�1@���@�dZ@��P@�S�@�33@��H@��+@��@���@���@�@�hs@��@���@�r�@� �@��@�33@�@�M�@�@�x�@�hs@�O�@�V@���@��D@�Z@�1'@� �@���@���@�K�@���@�@��^@���@���@�&�@��/@�r�@�1@���@�33@��R@��#@�Q�@��F@��P@�l�@��y@�n�@�M�@�{@��#@���@�`B@�%@��u@�r�@�Z@�1'@�  @��
@��w@�&�@�t�@}��@s"�@g��@\��@U�@J�@D(�@=�h@7+@/�w@*�\@%��@�@��@��@o@�+@�
@�9111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B1B-B9XB?}BB�BI�BZB]/B[#BVBP�BM�BK�BG�BC�BE�BQ�B]/B_;BaHBl�Bs�B}�B��B��B�BƨBȴBɺBȴBŢB�'B��B��B�oB~�Bv�B^5B?}B6FB.B �B��B�ZB��B�FB�B��B��B��B��B�{B�%Br�B]/BF�B"�B�BJBB��B�#B��B�FB�!B�'B�B��B�bB�7B�%B�Bs�BffB`BBS�BK�B?}B2-B)�B �B�BhBB
�B
�`B
�5B
��B
�XB
�B
�B
��B
��B
��B
�oB
y�B
iyB
^5B
J�B
5?B
�B

=B	��B	�B	�5B	��B	��B	ĜB	�^B	�?B	�B	��B	��B	�7B	�B	~�B	x�B	o�B	dZB	YB	M�B	E�B	>wB	8RB	/B	'�B	!�B	�B	oB	JB	B��B��B��B�B�TB�;B�5B�/B�)B�#B�B�B��B��B��B��B��B��BȴBĜBÖBB��B�qB�RB�?B�9B�-B�B�B��B��B��B��B��B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�'B�9B�XB�FB�LB�jB�wB�wB��B��BÖBƨBȴBɺB��B��B��B��B��B��B��B��B��B��B��B��B�B�#B�B�5B�;B�HB�BB�TB�B��B��B��B��B��B��B��BɺB��B��B��B�;B�B��B	B	B	%B	uB	�B	"�B	%�B	%�B	�B	�B	,B	.B	0!B	49B	8RB	:^B	;dB	=qB	A�B	>wB	9XB	8RB	E�B	VB	ZB	[#B	ZB	ZB	YB	W
B	S�B	P�B	K�B	D�B	E�B	H�B	K�B	F�B	M�B	[#B	]/B	^5B	^5B	\)B	_;B	bNB	e`B	dZB	aHB	aHB	ffB	cTB	aHB	bNB	dZB	ffB	ffB	hsB	iyB	iyB	hsB	dZB	ffB	cTB	T�B	K�B	F�B	F�B	F�B	_;B	l�B	o�B	t�B	z�B	}�B	� B	�B	}�B	{�B	}�B	� B	�+B	�+B	�B	�B	� B	� B	� B	� B	}�B	|�B	�B	�B	�7B	�+B	�B	�B	�B	�B	�B	�%B	�%B	�B	�B	�B	}�B	{�B	�B	�PB	��B	��B	��B	��B	��B	��B	��B	�-B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�-B	�?B	�LB	�^B	�jB	�jB	�wB	�wB	�}B	�}B	��B	��B	B	ÖB	ÖB	ĜB	ƨB	ǮB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�)B	�/B	�5B	�BB	�NB	�TB	�TB	�ZB	�fB	�mB	�sB	�sB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
VB
�B
$�B
1'B
8RB
>wB
F�B
K�B
P�B
VB
[#B
]/B
bNB
gmB
m�B
p�B
s�B
x�B
z�B
~�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B9B-B9dB?�BB�BI�BZ%B]<B[,BVBP�BM�BK�BG�BC�BE�BQ�B]9B_CBaSBl�Bs�B~B��B��B�BƵB��B��B��BųB�8B��B��B�{B	Bv�B^AB?�B6SB.B �B��B�cB��B�OB�B��B��B��B��B��B�/Br�B]7BF�B"�B�BRB$B��B�+B��B�OB�,B�1B�"B��B�lB�AB�1B�Bs�BfpB`MBS�BK�B?�B29B*B �B�BqBB
�B
�lB
�BB
��B
�bB
�&B
�B
��B
��B
��B
�zB
y�B
i�B
^AB
J�B
5PB
�B

OB	��B	�B	�IB	��B	��B	ĲB	�rB	�QB	�/B	��B	��B	�MB	�#B	B	x�B	o�B	dqB	Y/B	M�B	E�B	>�B	8kB	/3B	(B	!�B	�B	�B	cB	5B�B��B��B�B�qB�XB�RB�KB�IB�?B�3B�"B�B�B�	B�B��B��B��BĻBôB­B��B��B�oB�^B�WB�MB�;B�&B�B�B�B��B�B�(B�B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�EB�VB�vB�eB�kB��B��B��B��B��BôB��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�3B�>B�5B�QB�UB�eB�_B�qB�,B�B�B��B��B��B�B��B��B��B��B��B�XB��B�B	'B	9B	?B	�B	�B	"�B	%�B	%�B	�B	�B	,B	.,B	09B	4RB	8hB	:vB	;B	=�B	A�B	>�B	9pB	8jB	E�B	VB	Z3B	[;B	Z6B	Z6B	Y/B	W B	TB	P�B	K�B	D�B	E�B	H�B	K�B	F�B	M�B	[;B	]EB	^JB	^LB	\?B	_TB	beB	euB	dqB	a`B	a`B	f|B	cmB	a^B	beB	dpB	f~B	f{B	h�B	i�B	i�B	h�B	drB	f{B	cjB	UB	K�B	F�B	F�B	F�B	_TB	l�B	o�B	t�B	z�B	~
B	�B	�$B	~B	{�B	~
B	�B	�?B	�AB	�)B	�B	�B	�B	�B	�B	~	B	}B	�/B	�5B	�MB	�BB	�.B	�!B	�$B	�/B	�1B	�8B	�:B	�/B	�(B	�#B	~
B	{�B	�!B	�eB	��B	��B	��B	��B	��B	��B	��B	�BB	�.B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�#B	�"B	�*B	�6B	�AB	�SB	�aB	�qB	�B	��B	��B	��B	��B	��B	��B	��B	¡B	êB	éB	įB	ƾB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	�	B	�B	�B	�#B	�*B	�0B	�<B	�DB	�JB	�WB	�cB	�fB	�hB	�oB	�zB	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	��B	� B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B
-B
iB
�B
$�B
17B
8eB
>�B
F�B
K�B
P�B
VB
[4B
]=B
b]B
g{B
m�B
p�B
s�B
x�B
z�B
111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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