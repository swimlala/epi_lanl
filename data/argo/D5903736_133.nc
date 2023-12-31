CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-12-14T20:16:18Z AOML 3.0 creation; 2016-05-31T19:14:46Z UW 3.1 conversion     
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
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20151214201618  20160531121446  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4051_7090_133                   2C  D   APEX                            5368                            041511                          846 @׆k�D�1   @׆���@4I7KƧ��dax���1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   A   A   @���@�  A   A   A@  A^ffA�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�33B�  B���B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C?�fCA�fCD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@y�DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DG��DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�fDy��D�fD�<�D��3D��3D�fD�C3D���D�ٚD�fD�I�D�|�D�� D�fD�,�Dڙ�D�� D�fD�P D� D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��R@��@��A�\A>�\A\��A~�\A�G�A�G�A�G�A�G�A�G�A�G�A�G�A�G�B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B���B���B���B���B�B���B���B���B���B���B�B���B���B���B���B���B���B���B���B���B�B���B���B���B���B���B���B���B���B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?�\CA�\CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{D z=D �=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=D	z=D	�=D
z=D
�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=D z=D �=D!z=D!�=D"z=D"�=D#z=D#�=D$z=D$�=D%z=D%�=D&z=D&�=D'z=D'�=D(z=D(�=D)z=D)�=D*z=D*�=D+z=D+�=D,z=D,�=D-z=D-�=D.z=D.�=D/z=D/�=D0z=D0�=D1z=D1�=D2z=D2�=D3z=D3�=D4z=D4�=D5z=D5�=D6z=D6�=D7z=D7�=D8z=D8�=D9z=D9�=D:z=D:�=D;z=D;�=D<z=D<�=D=z=D=�=D>z=D>�=D?z=D?�=D@s�D@�=DAz=DA�=DBz=DB�=DCz=DC�=DDz=DD�=DEz=DE�=DFz=DF�=DGz=DG��DHz=DH�=DIz=DI�=DJz=DJ�=DKz=DK�=DLz=DL�=DMz=DM�=DNz=DN�=DOz=DO�=DPz=DP�=DQz=DQ�=DRz=DR�=DSz=DS�=DTz=DT�=DUz=DU�=DVz=DV�=DWz=DW�=DXz=DX�=DYz=DY�=DZz=DZ�=D[z=D[�=D\z=D\�=D]z=D]�=D^z=D^�=D_z=D_�=D`z=D`�=Daz=Da�=Dbz=Db�=Dcz=Dc�=Ddz=Dd�=Dez=De�=Dfz=Df�=Dgz=Dg�=Dhz=Dh�=Diz=Di�=Djz=Dj�=Dkz=Dk�=Dlz=Dl�=Dmz=Dm�=Dnz=Dn�=Doz=Do�=Dpz=Dp�=Dqz=Dq�=Drz=Dr�=Dsz=Ds�=Dtz=Dt�Dy�
D��D�9�D��RD��RD��D�@RD���D�ֹD��D�F�D�y�D��D��D�)�Dږ�D�D��D�MD�}D��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A֗�A֙�A֟�A֟�A֟�A֣�A֧�A֡�A֧�A֬Aְ!AֶFA���A־wA�ƨA��A���A�A��TA֡�A֓uAօA։7A֮A֣�A֓uA�t�A�VA�?}A�/A�
=A��yA��A��`Aե�A�|�AՋDAէ�A�Aպ^A�r�A��/Aѩ�A��;Ȁ\Aˇ+AɬA�A���A�K�A���A���A���A�;dA��A�(�A��uA�n�A��\A�A���A��mA�M�A�|�A�5?A�5?A��TA���A��A���A�/A��TA�=qA���A��A��^A��DA�t�A��A���A�hsA��DA���A�jA�`BA�^5A�l�A�(�A�ƨA���A�JA��A�"�A�`BA�7LA��-A�bA���A�~�A��A�A�A�p�A�/A�
=A� �A�O�A�=qA���A~5?A{x�Ax$�Au"�Aq��AodZAm%Ail�Ag�Ae��Ac�7AbAaG�A`Q�A]�hAZ��AZAWVAU�TAU��AU�AS?}AP�\APbAN��AL��AL5?AJ�/AHA�AF��AD�RAB�AA�FA?�A>bNA<��A;7LA9��A9t�A8��A7K�A6(�A4~�A1��A0 �A.��A-C�A+l�A*-A)33A( �A&ȴA%"�A$9XA"�yA!+A JA&�A��AK�A�\A�yAVA�-A�HA1'A1A��A�A|�A�\AbAƨA�Ax�A/A�+A-A�PA�DA��A
�\A	��A	?}An�AM�Al�A�uAbAp�A�jAVA�A�A �@��R@�z�@���@�%@���@���@��@�E�@��`@��@�z�@띲@�@��/@��@旍@�@�1@�+@���@�p�@�&�@�9X@�
=@��@ڰ!@�M�@��@�/@׮@և+@�p�@�Ĝ@�j@���@Ӆ@�V@ѩ�@��@ύP@�
=@�@��@�ȴ@��@̃@��
@��@��@�~�@���@�%@��
@�+@Ɨ�@�@őh@�x�@�r�@Å@�-@��@���@��@���@���@�G�@��@��j@�Z@��m@�ƨ@�
=@���@�^5@��-@��@� �@��
@�K�@�ȴ@���@��^@���@���@��;@��R@�ff@�@��7@�x�@�p�@�G�@���@�  @���@��F@���@�\)@���@�v�@��@���@�O�@��@�A�@��@�  @���@�|�@�S�@�;d@��@�^5@�@��@��7@�&�@��@��w@�|�@�E�@��-@��7@�O�@���@�z�@�I�@�(�@�b@�(�@�  @��P@�33@�
=@��y@��@��@��R@���@�M�@��T@�x�@�7L@��`@�Ĝ@�I�@�  @�ƨ@��@���@�t�@�K�@�o@��y@���@�ff@�{@��h@�p�@�X@�?}@��@��`@��9@�bN@���@���@���@��@�C�@��@��@��R@�V@�$�@��@�J@��#@��-@�O�@�V@���@�j@�A�@�(�@�  @���@�S�@�33@�+@�@�~�@�V@�E�@�-@��@���@�hs@�?}@�7L@��@���@��9@�Ĝ@��`@��`@���@���@��j@��D@�1'@�ƨ@���@��P@�|�@���@�M�@�5?@��@���@��^@�`B@���@��D@�1@��;@�\)@�+@��R@�-@���@���@���@�`B@��@���@��/@���@��9@��@�Z@�(�@��@��
@�ƨ@��w@���@�"�@��@�ȴ@��\@�n�@�M�@��@��@��T@��-@���@���@�p�@��@��D@�Q�@��@��
@��w@�K�@���@�V@���@���@�@�p�@�O�@��@��@�1'@�ƨ@�t�@�l�@�;d@�ȴ@��\@�J@���@���@{@uV@l1@c��@Xr�@Nv�@I%@EV@?�@7\)@1��@)�@%O�@ b@�@1'@?}@�@�h@
^5111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A֗�A֙�A֟�A֟�A֟�A֣�A֧�A֡�A֧�A֬Aְ!AֶFA���A־wA�ƨA��A���A�A��TA֡�A֓uAօA։7A֮A֣�A֓uA�t�A�VA�?}A�/A�
=A��yA��A��`Aե�A�|�AՋDAէ�A�Aպ^A�r�A��/Aѩ�A��;Ȁ\Aˇ+AɬA�A���A�K�A���A���A���A�;dA��A�(�A��uA�n�A��\A�A���A��mA�M�A�|�A�5?A�5?A��TA���A��A���A�/A��TA�=qA���A��A��^A��DA�t�A��A���A�hsA��DA���A�jA�`BA�^5A�l�A�(�A�ƨA���A�JA��A�"�A�`BA�7LA��-A�bA���A�~�A��A�A�A�p�A�/A�
=A� �A�O�A�=qA���A~5?A{x�Ax$�Au"�Aq��AodZAm%Ail�Ag�Ae��Ac�7AbAaG�A`Q�A]�hAZ��AZAWVAU�TAU��AU�AS?}AP�\APbAN��AL��AL5?AJ�/AHA�AF��AD�RAB�AA�FA?�A>bNA<��A;7LA9��A9t�A8��A7K�A6(�A4~�A1��A0 �A.��A-C�A+l�A*-A)33A( �A&ȴA%"�A$9XA"�yA!+A JA&�A��AK�A�\A�yAVA�-A�HA1'A1A��A�A|�A�\AbAƨA�Ax�A/A�+A-A�PA�DA��A
�\A	��A	?}An�AM�Al�A�uAbAp�A�jAVA�A�A �@��R@�z�@���@�%@���@���@��@�E�@��`@��@�z�@띲@�@��/@��@旍@�@�1@�+@���@�p�@�&�@�9X@�
=@��@ڰ!@�M�@��@�/@׮@և+@�p�@�Ĝ@�j@���@Ӆ@�V@ѩ�@��@ύP@�
=@�@��@�ȴ@��@̃@��
@��@��@�~�@���@�%@��
@�+@Ɨ�@�@őh@�x�@�r�@Å@�-@��@���@��@���@���@�G�@��@��j@�Z@��m@�ƨ@�
=@���@�^5@��-@��@� �@��
@�K�@�ȴ@���@��^@���@���@��;@��R@�ff@�@��7@�x�@�p�@�G�@���@�  @���@��F@���@�\)@���@�v�@��@���@�O�@��@�A�@��@�  @���@�|�@�S�@�;d@��@�^5@�@��@��7@�&�@��@��w@�|�@�E�@��-@��7@�O�@���@�z�@�I�@�(�@�b@�(�@�  @��P@�33@�
=@��y@��@��@��R@���@�M�@��T@�x�@�7L@��`@�Ĝ@�I�@�  @�ƨ@��@���@�t�@�K�@�o@��y@���@�ff@�{@��h@�p�@�X@�?}@��@��`@��9@�bN@���@���@���@��@�C�@��@��@��R@�V@�$�@��@�J@��#@��-@�O�@�V@���@�j@�A�@�(�@�  @���@�S�@�33@�+@�@�~�@�V@�E�@�-@��@���@�hs@�?}@�7L@��@���@��9@�Ĝ@��`@��`@���@���@��j@��D@�1'@�ƨ@���@��P@�|�@���@�M�@�5?@��@���@��^@�`B@���@��D@�1@��;@�\)@�+@��R@�-@���@���@���@�`B@��@���@��/@���@��9@��@�Z@�(�@��@��
@�ƨ@��w@���@�"�@��@�ȴ@��\@�n�@�M�@��@��@��T@��-@���@���@�p�@��@��D@�Q�@��@��
@��w@�K�@���@�V@���@���@�@�p�@�O�@��@��@�1'@�ƨ@�t�@�l�@�;d@�ȴ@��\@�J@���@���@{@uV@l1@c��@Xr�@Nv�@I%@EV@?�@7\)@1��@)�@%O�@ b@�@1'@?}@�@�h@
^5111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
ÖB
ÖB
ĜB
ŢB
ƨB
ǮB
ɺB
ȴB
ȴB
��B
��B
�B
�B
�B
��BBoB�B�BhBuB{B�B7LBT�BcTB�B�oB�oB�hB�hB�hB��B��B�hB�VB��B��B�LB�TBQ�B�B�NB/BJ�BbNBr�Bt�By�By�Bt�Bp�Bn�BjBdZBYBK�BF�BC�B>wB8RB-B�B�BVB+BB��B�fB��BŢB�RB�'B��B��B�B� Bq�BjBe`B`BBQ�BB�B<jB'�B�BB�B�B�)B��B�LB�B��B�Bu�BiyBdZB\)BC�B&�B�BB
�TB
��B
ǮB
�FB
��B
|�B
e`B
T�B
;dB
�B
	7B	��B	�BB	��B	ƨB	�XB	�!B	�B	��B	��B	�B	y�B	ffB	^5B	`BB	aHB	YB	J�B	D�B	:^B	,B	$�B	�B	
=B	B��B�B�B�ZB�/B�B��B��B��B��BƨBÖB��B�dB�9B�3B�?B�!B�B��B��B��B��B��B�uB�VB�DB�1B�%B�1B�B�B}�By�By�B� B�B�+B�%B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B~�B|�B}�B|�B|�B|�B{�B{�By�Bx�Bw�Bs�Bq�Bq�Bu�Bx�Bx�Bx�Bx�Bv�Br�Bq�Bo�Bo�Br�Bs�Bu�Bu�Bs�Bv�Bz�Bz�Bz�By�Bx�B{�Bz�By�Bx�Bw�Bv�Bw�By�B|�B~�B�B�B�+B�PB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�3B�?B�FB�XB�wB��BȴBȴBȴBȴB��B�B�
B�B�B�)B�HB�`B�B�B�B��B��B	  B	B	+B	+B	JB	PB	hB	{B	�B	�B	�B	!�B	'�B	,B	-B	/B	0!B	1'B	6FB	9XB	<jB	?}B	A�B	C�B	F�B	J�B	P�B	S�B	T�B	VB	W
B	ZB	[#B	\)B	_;B	bNB	dZB	e`B	gmB	iyB	k�B	n�B	o�B	r�B	t�B	u�B	u�B	w�B	y�B	z�B	{�B	� B	�%B	�JB	�\B	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�'B	�-B	�9B	�FB	�FB	�LB	�LB	�RB	�XB	�^B	�jB	�}B	��B	��B	��B	B	ÖB	ĜB	ŢB	ǮB	ǮB	ȴB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�#B	�#B	�/B	�;B	�BB	�HB	�NB	�NB	�NB	�TB	�TB	�ZB	�`B	�fB	�mB	�mB	�sB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
%B
%B
+B
+B
+B
%B
+B
%B
%B
1B
	7B

=B

=B
JB
PB
VB
VB
VB
VB
\B
bB
bB
bB
hB
{B
�B
�B
'�B
)�B
2-B
:^B
?}B
D�B
K�B
O�B
T�B
[#B
aHB
ffB
k�B
o�B
r�B
v�B
x�B
{�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
ßB
àB
ĨB
ŮB
ƲB
ǹB
��B
ȾB
ȾB
��B
��B
�B
�B
�B
��B#BzB�B�BoBB�B�B7TBUBc[B�B�xB�vB�nB�nB�nB��B��B�oB�]B��B��B�SB�\BQ�B�B�YB/%BJ�Bb[Br�Bt�By�By�Bt�Bp�Bn�Bj�BdfBY"BK�BF�BC�B>�B8_B-B�B�BbB6BB��B�pB� BŭB�]B�0B� B��B�(B�Bq�Bj�BeeB`GBQ�BB�B<sB'�B�B'B�B�B�1B��B�UB�B��B�Bu�Bi�Bd^B\/BC�B&�B�B"B
�aB
� B
ǹB
�TB
��B
|�B
emB
UB
;uB
�B
	GB	��B	�WB	�B	ƼB	�kB	�4B	�B	�B	��B	�&B	y�B	fzB	^LB	`YB	a^B	Y0B	J�B	D�B	:sB	, B	$�B	�B	
XB	5B��B�B�B�vB�KB�9B�B�B��B��B��BõB��B��B�WB�QB�_B�>B� B�B��B��B��B��B��B�vB�dB�SB�FB�SB�?B�,B~By�By�B� B�>B�KB�EB�@B�?B�@B�AB�=B�=B�?B�?B�9B�3B�3B�3B�,B�2B�-B�(BB}B~B}B}B}B|B|By�Bx�Bw�Bs�Bq�Bq�Bu�Bx�Bx�Bx�Bx�Bv�Br�Bq�Bo�Bo�Br�Bs�Bu�Bu�Bs�Bv�B{B{B{By�Bx�B|B{ By�Bx�Bw�Bv�Bw�By�B}BB�(B�,B�MB�oB��B��B��B��B��B��B��B��B��B��B��B��B��B�	B� B�3B�PB�\B�dB�tB��B��B��B��B��B��B�B� B�$B�0B�9B�EB�dB�yB�B�B��B�	B�B	 B	3B	FB	DB	cB	lB	�B	�B	�B	�B	�B	!�B	(B	,!B	-&B	/6B	0;B	1=B	6^B	9pB	<�B	?�B	A�B	C�B	F�B	J�B	P�B	TB	UB	VB	W"B	Z4B	[<B	\BB	_SB	bfB	dqB	ewB	g�B	i�B	k�B	n�B	o�B	r�B	t�B	u�B	u�B	w�B	y�B	z�B	{�B	�B	�;B	�]B	�rB	�|B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�#B	�'B	�;B	�AB	�MB	�ZB	�[B	�`B	�^B	�gB	�kB	�sB	�~B	��B	��B	��B	��B	¤B	êB	įB	ŸB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	�B	�B	�B	�
B	�B	�B	�B	�B	�B	�B	�)B	�0B	�6B	�6B	�AB	�MB	�UB	�]B	�aB	�cB	�cB	�gB	�jB	�mB	�tB	�{B	�B	�}B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	� B	�B	�B
 B
 B
B
%B
-B
.B
1B
6B
8B
AB
AB
>B
6B
@B
8B
9B
EB
	FB

OB

MB
ZB
aB
hB
hB
iB
iB
oB
tB
uB
uB
yB
�B
�B
�B
( B
*B
2?B
:oB
?�B
D�B
K�B
O�B
UB
[1B
aWB
fvB
k�B
o�B
r�B
v�B
x�B
{�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.09 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311214462016053112144620160531121446  AO  ARCAADJP                                                                    20151214201618    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20151214201618  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20151214201618  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531121446  IP                  G�O�G�O�G�O�                