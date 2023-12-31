CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2017-10-01T00:01:15Z creation      
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
_FillValue                    �,Argo profile    3.1 1.2 19500101000000  20171001000115  20190604094029  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4051                            2C  D   APEX                            5368                            041511                          846 @�'�(!1   @�'�_�]�@6�Q��d��l�C�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0ffB8  B@  BH  BP  BW��B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CQ�fCT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn�Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� DtٚDy�RD� �D�4�D��{D�ȤD��)D�C3D�p�D�a�D��D�I�D��\D���D�\D�-qD�x�D�)D�
D�?
D�yH11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��R@��@��A�\A>�\A^�\A~�\A�G�A�G�A�G�A�G�A�G�A�G�A�G�A�G�B��B��B��B��B'��B0
=B7��B?��BG��BO��BW=qB_��Bg��Bo��Bw��B��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B�B���B���B���B���B���B���B���B���B���B���B���B�B���B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ�\CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cn�Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C�HC��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{D z=D �=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=D	z=D	�=D
z=D
�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=D z=D �=D!z=D!�=D"z=D"�=D#z=D#�=D$z=D$�=D%z=D%�=D&z=D&�=D'z=D'�=D(z=D(�=D)z=D)�=D*z=D*�=D+z=D+�=D,z=D,�=D-z=D-�=D.z=D.�=D/z=D/�=D0z=D0�=D1z=D1�=D2z=D2�=D3z=D3�=D4z=D4�=D5z=D5�=D6z=D6�=D7z=D7�=D8z=D8�=D9z=D9�=D:z=D:�=D;z=D;�=D<z=D<�=D=z=D=�=D>z=D>�=D?z=D?�=D@z=D@�=DAz=DA�=DBz=DB�=DCz=DC�=DDz=DD�=DEz=DE�=DFz=DF�=DGz=DG�=DHz=DH�=DIz=DI�=DJz=DJ�=DKz=DK�=DLz=DL�=DMz=DM�=DNz=DN�=DOz=DO�=DPz=DP�=DQz=DQ�=DRz=DR�=DSz=DS�=DTz=DT�=DUz=DU�=DVz=DV�=DWz=DW�=DXz=DX�=DYz=DY�=DZz=DZ�=D[z=D[�=D\z=D\�=D]z=D]�=D^z=D^�=D_z=D_�=D`z=D`�=Daz=Da�=Dbz=Db�=Dcz=Dc�=Ddz=Dd�=Dez=De�=Dfz=Df�=Dgz=Dg�=Dhz=Dh�=Diz=Di�=Djz=Dj�=Dkz=Dk�=Dlz=Dl�=Dmz=Dm�=Dnz=Dn�=Doz=Do�=Dpz=Dp�=Dqz=Dq�=Drz=Dr�=Dsz=Ds�=Dtz=Dt��Dy��D���D�1�D���D���D��HD�@RD�m�D�^�D��D�GD��{DǿD�{D�*�D�vD�HD�)D�<)D�vg11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A��
A��/A��/A��#A��;A��;A��/A��HA��HA��HA��HA��HA��;A���AᛦA�A�`BA�x�AލPA��AݸRA�K�A��A�ȴA�?}AؼjA؟�A؍PAן�A��yA���A՛�A�"�A���A�9XA�-AсAсA���A�AΙ�A�~�Aʡ�A���A���A���A�Q�A�x�A�$�A��TA�$�A�bNA�A�XA��A�VA�1'A�I�A���A�\)A��mA��
A�p�A�$�A��DA�x�A���A���A�dZA�~�A��;A�
=A���A�~�A�r�A�~�A���A�G�A��yA�1'A���A���A�A�Q�A��HA���A�x�A���A��hA���A��DA��^A���A�ƨA��FA���A�bA�\)A�VA��A�$�A��A��A�  A�~�A���A���A�+A���A�p�A��A��jA�VA��A�1A��-A�l�A�?}A��A���A�l�A���A��9A���A~�DA}��A|�A{�Ax�`Av�ArJAl �Aj-Ah�Ag�Aep�Ab$�A`��A^VA\5?AZbAW�-AT=qARbAN�AL�ALn�AL$�AJ�9AH��AGO�AFȴAE�TAE��AD�yAD�ACAA��A?�7A>z�A=
=A;��A;�FA:Q�A9�-A8=qA6M�A4��A45?A3\)A2�A1�A0 �A/C�A.�A.�/A-"�A+��A*�A)��A(��A( �A'+A%x�A#A#��A#S�A!��A �A $�A��A�A{AK�A5?AVA�TAhsA
=A��A�TAffA?}A�DA��A�HA5?A$�A�A  A�A�mA�wAXAȴA5?A�TAXA;dA/A�/AE�A�/A1'A�A�wA
�/A	/A��A��A�A��A�\A��A33A Z@���@�(�@�
=@���@�E�@��^@���@�C�@�z�@�1'@���@��@�C�@��@�V@�7L@�z�@���@���@�V@� �@�5?@�7@���@��@�@�bN@��m@�F@�@��@��T@��`@߅@�G�@ڰ!@�5?@�@��#@���@ٺ^@ّh@��@�Q�@��y@�O�@�bN@�+@��@ҟ�@�@��`@�Z@Ϯ@θR@͉7@ʏ\@�M�@�J@ɺ^@ȃ@���@Ł@Ĵ9@�Q�@��@���@�l�@�V@�G�@�bN@�K�@�n�@��y@�~�@��T@��@�%@���@��9@�9X@��w@���@�7L@��j@�r�@��m@���@���@���@��+@�x�@�z�@�Q�@��@���@�"�@�5?@�G�@���@���@�1@�;d@��H@���@�V@�J@�`B@�%@���@�I�@��@�
=@���@�=q@�V@���@��P@�\)@��@�-@���@�X@�%@��D@�1@���@���@�-@��^@��h@��7@��@�x�@�O�@�?}@�%@���@���@��@�r�@��@��F@��@�~�@�ff@�V@�5?@�{@��#@�hs@��9@�j@� �@��
@��@�S�@�;d@�@�ȴ@�n�@�E�@��@��^@�X@�%@��`@��9@��@�z�@�j@�b@��;@�ƨ@���@�t�@�K�@�C�@�"�@��@�@��y@��@��H@��@�ȴ@���@��\@�n�@�$�@���@�p�@�V@���@�j@�  @�1'@�r�@�9X@��@��
@��
@��F@��F@���@�t�@�l�@�\)@�
=@�ȴ@��+@�V@�J@���@��h@��h@�`B@�&�@���@��`@��j@�Z@�1'@� �@�  @���@��P@�dZ@�;d@�+@��@�@��H@���@���@�~�@�^5@�M�@�J@�hs@�7L@���@��@��/@��j@��@���@�r�@�(�@�1@�  @��@��@�͟@��@u/@o�V@g�r@`�9@Y5�@P%�@I|@B�"@=�@7J#@21�@)��@"�@)�@1�@m�@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111   A���A��
A��/A��/A��#A��;A��;A��/A��HA��HA��HA��HA��HA��;A���AᛦA�A�`BA�x�AލPA��AݸRA�K�A��A�ȴA�?}AؼjA؟�A؍PAן�A��yA���A՛�A�"�A���A�9XA�-AсAсA���A�AΙ�A�~�Aʡ�A���A���A���A�Q�A�x�A�$�A��TA�$�A�bNA�A�XA��A�VA�1'A�I�A���A�\)A��mA��
A�p�A�$�A��DA�x�A���A���A�dZA�~�A��;A�
=A���A�~�A�r�A�~�A���A�G�A��yA�1'A���A���A�A�Q�A��HA���A�x�A���A��hA���A��DA��^A���A�ƨA��FA���A�bA�\)A�VA��A�$�A��A��A�  A�~�A���A���A�+A���A�p�A��A��jA�VA��A�1A��-A�l�A�?}A��A���A�l�A���A��9A���A~�DA}��A|�A{�Ax�`Av�ArJAl �Aj-Ah�Ag�Aep�Ab$�A`��A^VA\5?AZbAW�-AT=qARbAN�AL�ALn�AL$�AJ�9AH��AGO�AFȴAE�TAE��AD�yAD�ACAA��A?�7A>z�A=
=A;��A;�FA:Q�A9�-A8=qA6M�A4��A45?A3\)A2�A1�A0 �A/C�A.�A.�/A-"�A+��A*�A)��A(��A( �A'+A%x�A#A#��A#S�A!��A �A $�A��A�A{AK�A5?AVA�TAhsA
=A��A�TAffA?}A�DA��A�HA5?A$�A�A  A�A�mA�wAXAȴA5?A�TAXA;dA/A�/AE�A�/A1'A�A�wA
�/A	/A��A��A�A��A�\A��A33A Z@���@�(�@�
=@���@�E�@��^@���@�C�@�z�@�1'@���@��@�C�@��@�V@�7L@�z�@���@���@�V@� �@�5?@�7@���@��@�@�bN@��m@�F@�@��@��T@��`@߅@�G�@ڰ!@�5?@�@��#@���@ٺ^@ّh@��@�Q�@��y@�O�@�bN@�+@��@ҟ�@�@��`@�Z@Ϯ@θR@͉7@ʏ\@�M�@�J@ɺ^@ȃ@���@Ł@Ĵ9@�Q�@��@���@�l�@�V@�G�@�bN@�K�@�n�@��y@�~�@��T@��@�%@���@��9@�9X@��w@���@�7L@��j@�r�@��m@���@���@���@��+@�x�@�z�@�Q�@��@���@�"�@�5?@�G�@���@���@�1@�;d@��H@���@�V@�J@�`B@�%@���@�I�@��@�
=@���@�=q@�V@���@��P@�\)@��@�-@���@�X@�%@��D@�1@���@���@�-@��^@��h@��7@��@�x�@�O�@�?}@�%@���@���@��@�r�@��@��F@��@�~�@�ff@�V@�5?@�{@��#@�hs@��9@�j@� �@��
@��@�S�@�;d@�@�ȴ@�n�@�E�@��@��^@�X@�%@��`@��9@��@�z�@�j@�b@��;@�ƨ@���@�t�@�K�@�C�@�"�@��@�@��y@��@��H@��@�ȴ@���@��\@�n�@�$�@���@�p�@�V@���@�j@�  @�1'@�r�@�9X@��@��
@��
@��F@��F@���@�t�@�l�@�\)@�
=@�ȴ@��+@�V@�J@���@��h@��h@�`B@�&�@���@��`@��j@�Z@�1'@� �@�  @���@��P@�dZ@�;d@�+@��@�@��H@���@���@�~�@�^5@�M�@�J@�hs@�7L@���@��@��/@��j@��@���@�r�@�(�@�1@�  G�O�@��@�͟@��@u/@o�V@g�r@`�9@Y5�@P%�@I|@B�"@=�@7J#@21�@)��@"�@)�@1�@m�@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
=B
=B	7B	7B	7B	7B	7B	7B
=B	7B	7B	7B	7B	7B1B%BBB
�B
ÖB
��B
�wB
��B
�B
��BbB
ŢB
�/B
��B>wB]/Bn�Bp�Bk�BaHBXBVBl�Bz�Br�Bo�B�7B�{Bp�BaHBR�B�RB��B�B,BB�BD�BE�BD�BO�BR�BI�B^5Bz�Bz�Bk�BcTBu�B�B�B�B|�Bn�Bk�B��B��B��B��B��B��B��B��B��B��B��B��B�bB�DB�DB�DB�1B�B~�Bs�Bk�BgmBdZB^5BS�BM�BD�B?}B5?B)�B�B��B�B��B��B�Bs�BffB[#BK�BB�B>wB8RB2-B)�B�B1B
��B
�B
�B
�B
�qB
�FB
�B
��B
u�B
dZB
]/B
R�B
I�B
9XB
(�B
+B	�BB	��B	ǮB	�wB	�B	��B	�bB	�B	u�B	gmB	YB	H�B	<jB	.B	&�B	#�B	�B	�B	hB	PB	DB	1B	B	B	  B��B��B�B�B�B�B�yB�sB�fB�BB�B��B��B��B��BɺBBÖBÖBÖB�wB�LB�'B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�oB�hB�bB�bB�bB�bB�bB�\B�\B�\B�VB�PB�DB�1B�+B�7B�7B�7B�=B�7B�7B�+B�%B�%B�B�B~�By�By�By�By�B~�B~�B� B�%B�DB�JB�JB�JB�DB�=B�7B�DB�DB�JB�JB�JB�JB�DB�PB�\B�VB�VB�JB�=B�1B�+B�7B�=B�7B�7B�=B�=B�=B�DB�JB�=B�+B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�%B�B�B�B�B�B�B�B�+B�DB�VB�\B�bB�bB�hB��B��B��B��B��B�!B�?B�RB�XB�jB�qB�qB�}B��BÖBŢBƨBȴB��B��B��B��B�B�B�;B�TB�mB�B�B�B��B��B��B��B	B	B	B	B	%B	1B		7B	PB	bB	�B	�B	�B	�B	�B	#�B	#�B	"�B	#�B	'�B	+B	-B	/B	2-B	49B	6FB	;dB	?}B	?}B	?}B	?}B	>wB	>wB	@�B	A�B	A�B	A�B	C�B	F�B	G�B	J�B	M�B	R�B	YB	ZB	[#B	]/B	_;B	dZB	l�B	s�B	u�B	x�B	{�B	~�B	� B	�B	�B	�B	�1B	�=B	�DB	�bB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�-B	�3B	�9B	�?B	�FB	�RB	�jB	�wB	ĜB	ǮB	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�#B	�)B	�/B	�/B	�5B	�;B	�BB	�HB	�ZB	�fB	�fB	�mB	�mB	�sB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
#B
VB
&�B
1�B
6FB
<B
CB
JrB
OBB
S&B
X_B
[�B
b�B
d�B
j�B
o�B
s�B
x�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111   B
�
B
�
B
�B
�B
�B
�B
�B
�B
�
B
�B
�B
�B
�B
�B
� B
��B
��B
��B
�eB
�oB
�XB
�GB
��B
�~B
��B1B
�zB
�B
��B3EBQ�Bc`BelB`OBVBL�BJ�BaUBo�BgyBdcB}�B�BBenBVBG�B�B�B]B �B7MB9TB:[B9UBD�BG�B>tBR�Bo�Bo�B`BBX
BjzBu�Bu�Bu�Bq�BcSB`>B�9B�tB�}B��B��B��B��B��B��B�gB�kB�FB�B�B�B�B|�Bv�Bs�BhpB`FB\)BYBR�BH�BB�B9[B4<B*B�BJB�B�KB��B��Bx�Bh�B[:BO�B@�B7lB3QB-0B'B�B�B
�B
�B
�B
�iB
��B
�WB
�.B
��B
��B
j�B
YKB
R B
G�B
>�B
.QB
�B	�$B	�>B	��B	��B	�wB	�B	��B	�gB	wB	j�B	\wB	N"B	=�B	1xB	#%B	�B	�B	�B	�B	~B	dB	 ZB�EB�5B�*B�B��B��B�B�B�B�BޏB݊BۂB�ZB�,B�B�B�B��B��B��B��B��B��B��B�mB�HB�3B�$B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�|B�xB�kB}YB|SB~bB~^B~^BdB~^B~[B|SB{MB{MByABw4Bt$BoBoBoBoBt"Bt'Bu*B{PB�qB�qB�tB�uB�nBjB~`B�mB�mB�rB�vB�rB�uB�pB�zB��B��B�B�wBhB}`B|WB~bBkB~cB~cBhBfBiB�sB�tBkB|XBzGBx>Bx<Bx?Bx?BxABx:Bx=Bx<Bv4Bx?BxBByHBzIBzMBzMBzOBzNBzNB{SByFBw7Bw;Bw:Bw<Bw:Bx?B|WB�rB��B��B��B��B��B��B��B��B��B�B�LB�kB�yB��B��B��B��B��B��B��B��B��B��B��B�B�%B�)B�,B�>B�dB�{BܖB�B�B��B��B��B�B�B�<B�BB�EB�GB�HB�XB�`B	vB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	B	 &B	"1B	$=B	'PB	)\B	+iB	0�B	4�B	4�B	4�B	4�B	3�B	3�B	5�B	6�B	6�B	6�B	8�B	;�B	<�B	?�B	B�B	HB	N9B	O<B	PCB	RQB	TaB	YzB	a�B	h�B	j�B	m�B	qB	tB	uB	v%B	w)B	z<B	}MB	]B	�_B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�&B	�4B	�CB	�FB	�OB	�VB	�XB	�_B	�mB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	� B	�*B	�(B	�%B	�4B	�2B	�9B	�BB	�FB	�EB	�KB	�SB	�[B	�_B	�pB	�zB	�yB	܆B	܄B	݊B	ލB	ސB	ޏB	ߕB	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��G�O�B	�B
1B
=B
kB
�B
&�B
+ZB
1/B
8$B
?�B
DTB
H;B
MsB
P�B
W�B
Y�B
_�B
d�B
h�B
m�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.09 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9997(+/-0), vertically averaged dS =-0.011(+/-0.001) in PSS-78.                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Map scales: x=6,3; y=2,1. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                            201906040940292019060409402920190604094029  AO  ARCAADJP                                                                    20171001000115    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20171001000115  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20171001000115  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190604094029  IP                  G�O�G�O�G�O�                