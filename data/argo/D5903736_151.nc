CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-11-21T04:11:50Z creation      
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
_FillValue                    �,Argo profile    3.1 1.2 19500101000000  20181121041150  20190604094021  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4051                            2C  D   APEX                            5368                            041511                          846 @״,֩<�1   @״.�+�@2�$�/��d3����1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CG�fCJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:�fD;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�fDy��D��D�O\D���D��\D���D�@ D�ffD���D�
=D�O�D��D���D�\D�@RD�mD���D�RD�D{D�p 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��A�\A>�\A^�\A~�\A�G�A�G�A�G�A�G�A�G�A�G�A�G�A�G�B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B�B���B���B���B���B���B���B���B���B���B���B���B�B���B���B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG�\CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{C��{D z=D �=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D �Dz=D�=D	z=D	�=D
z=D
�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=Dz=D�=D z=D �=D!z=D!�=D"z=D"�=D#z=D#�=D$z=D$�=D%z=D%�=D&z=D&�=D'z=D'�=D(z=D(�=D)z=D)�=D*z=D*�=D+z=D+�=D,z=D,�=D-z=D-�=D.z=D.�=D/z=D/�=D0z=D0�=D1z=D1�=D2z=D2�=D3z=D3�=D4z=D4�=D5z=D5�=D6z=D6�=D7z=D7�=D8z=D8�=D9z=D9�=D:��D:�=D;z=D;�=D<z=D<�=D=z=D=�=D>z=D>�=D?z=D?�=D@z=D@�=DAz=DA�=DBz=DB�=DCz=DC�=DDz=DD�=DEz=DE�=DFz=DF�=DGz=DG�=DHz=DH�=DIz=DI�=DJz=DJ�=DKz=DK�=DLz=DL�=DMz=DM�=DNz=DN�=DOz=DO�=DPz=DP�=DQz=DQ�=DRz=DR�=DSz=DS�=DTz=DT�=DUz=DU�=DVz=DV�=DWz=DW�=DXz=DX�=DYz=DY�=DZz=DZ�=D[z=D[�=D\z=D\�=D]z=D]�=D^z=D^�=D_z=D_�=D`z=D`�=Daz=Da�=Dbz=Db�=Dcz=Dc�=Ddz=Dd�=Dez=De�=Dfz=Df�=Dgz=Dg�=Dhz=Dh�=Diz=Di�=Djz=Dj�=Dkz=Dk�=Dlz=Dl�=Dmz=Dm�=Dnz=Dn�=Doz=Do�=Dpz=Dp�=Dqz=Dq�=Drz=Dr�=Dsz=Ds�=Dtz=Dt�Dy~D��D�L{D���D��{D���D�=D�c�D���D�\D�L�D��3D���D�{D�=qD�j>D�� D�qD�A�D�m11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A׼jA׾wA���A���A�A�A�ĜA�A���A���A׺^A׺^Aן�A׋DA�oA֡�A�O�A�bA��
A�A�"�A��A���Aӥ�A�/A�bAуAиRA�G�A�|�A��AξwA��A�|�A�33A��Ȁ\A�XA�;dA˼jA��mAʅA�I�Aɺ^Aɏ\A�I�A���A�VA�VAǴ9A�=qAƶFA�E�Aş�AļjA��;A��A��jA�M�A���A�5?A�&�A�A���A��!A�5?A���A�A�A��A�A��A��A��A�jA���A�S�A�z�A��A���A��/A�C�A��A��A�bA���A��A�ffA�XA��A���A��yA��+A�jA��A��uA�S�A���A���A��A��A�M�A�XA�ffA�oA���A�1A��A� �A�I�A�z�A���A���A��A�ZA�bA��PA���A��9A�K�A��AzM�At�/AqAnĜAl��AiK�Af��Ac��Aa�A` �A]�mA\�A[�-AZ�yAXv�AVr�AUC�AR�RAOVALn�AKoAI�AH�yAG%AE�
AC��AB�`A@��A?&�A<v�A;C�A9ƨA7t�A4bA2�yA2v�A1��A0�jA/`BA.E�A-��A+��A*�!A*I�A)�^A)�FA)A(�A'�A&�/A&��A&�\A&I�A%�FA$(�A#��A#\)A"v�A!�A {A�hA�`A��AZA�A��AK�A�+A�A��AA33A5?A�A(�AXA�FAA�AdZA�uAhsA	�FA�A�-A�A��A��A��A�!A�/AXA~�A�A�TA?}A Ĝ@���@���@��@�9X@��@�t�@���@���@�@�ff@���@�V@�Ĝ@�z�@�K�@��@��/@띲@�M�@�u@�G�@�=q@��@އ+@�S�@ݙ�@ٙ�@�o@���@ղ-@�%@�Z@�/@�Z@�l�@Ο�@�J@��@ʟ�@ɺ^@��@Ȭ@�I�@��@�p�@�ƨ@�@�n�@�v�@�ff@���@�`B@���@���@�O�@�j@��@���@�V@���@��@�Q�@��@�dZ@��@�V@���@��@�Z@���@�t�@�;d@�o@�
=@�ȴ@�M�@�J@��@��^@���@� �@�S�@��H@�ȴ@�ff@��-@�%@��@��9@��@�z�@�Z@�9X@�9X@�1'@�b@�+@�@�{@��h@���@���@�I�@��F@��
@�|�@�l�@�K�@�dZ@��P@�dZ@��R@��^@��@�j@��;@��@�S�@���@���@���@�M�@�J@�@���@��7@�/@��@�Ĝ@��D@�1'@��
@��F@��@���@��P@�S�@��y@��@���@�n�@�n�@�5?@�&�@��u@�r�@��@��9@��m@�dZ@�ȴ@�-@���@�r�@�I�@�1'@�1@���@�S�@��\@�n�@�n�@�ff@�M�@�5?@�J@�@�hs@��@��/@��@��u@�Z@�(�@��@��m@��F@��@�dZ@�\)@��@���@�33@��H@��+@�=q@�-@�-@�5?@�-@�-@��@�@��@���@��-@���@���@��7@�?}@���@��u@��@�Q�@�b@���@�@��R@��R@��R@��+@�^5@�V@�V@�V@�=q@�$�@�~�@��H@���@��\@��T@��@��@���@�V@�V@��@��@�j@��@�b@� �@��
@���@�t�@�S�@�K�@�+@�ȴ@��R@���@���@��\@�n�@�n�@�^5@�V@�=q@�5?@�$�@�@��@�@���@�X@�V@���@���@��u@�1@��F@��P@�l�@�;d@��@���@��!@�~�@�E�@��T@��^@���@���@|(�@rh
@i�.@c��@Z�@T�$@L<�@FV@?�@8?�@1%F@+�4@&�6@!�T@M@Y@J#@+@�611111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111   A׼jA׾wA���A���A�A�A�ĜA�A���A���A׺^A׺^Aן�A׋DA�oA֡�A�O�A�bA��
A�A�"�A��A���Aӥ�A�/A�bAуAиRA�G�A�|�A��AξwA��A�|�A�33A��Ȁ\A�XA�;dA˼jA��mAʅA�I�Aɺ^Aɏ\A�I�A���A�VA�VAǴ9A�=qAƶFA�E�Aş�AļjA��;A��A��jA�M�A���A�5?A�&�A�A���A��!A�5?A���A�A�A��A�A��A��A��A�jA���A�S�A�z�A��A���A��/A�C�A��A��A�bA���A��A�ffA�XA��A���A��yA��+A�jA��A��uA�S�A���A���A��A��A�M�A�XA�ffA�oA���A�1A��A� �A�I�A�z�A���A���A��A�ZA�bA��PA���A��9A�K�A��AzM�At�/AqAnĜAl��AiK�Af��Ac��Aa�A` �A]�mA\�A[�-AZ�yAXv�AVr�AUC�AR�RAOVALn�AKoAI�AH�yAG%AE�
AC��AB�`A@��A?&�A<v�A;C�A9ƨA7t�A4bA2�yA2v�A1��A0�jA/`BA.E�A-��A+��A*�!A*I�A)�^A)�FA)A(�A'�A&�/A&��A&�\A&I�A%�FA$(�A#��A#\)A"v�A!�A {A�hA�`A��AZA�A��AK�A�+A�A��AA33A5?A�A(�AXA�FAA�AdZA�uAhsA	�FA�A�-A�A��A��A��A�!A�/AXA~�A�A�TA?}A Ĝ@���@���@��@�9X@��@�t�@���@���@�@�ff@���@�V@�Ĝ@�z�@�K�@��@��/@띲@�M�@�u@�G�@�=q@��@އ+@�S�@ݙ�@ٙ�@�o@���@ղ-@�%@�Z@�/@�Z@�l�@Ο�@�J@��@ʟ�@ɺ^@��@Ȭ@�I�@��@�p�@�ƨ@�@�n�@�v�@�ff@���@�`B@���@���@�O�@�j@��@���@�V@���@��@�Q�@��@�dZ@��@�V@���@��@�Z@���@�t�@�;d@�o@�
=@�ȴ@�M�@�J@��@��^@���@� �@�S�@��H@�ȴ@�ff@��-@�%@��@��9@��@�z�@�Z@�9X@�9X@�1'@�b@�+@�@�{@��h@���@���@�I�@��F@��
@�|�@�l�@�K�@�dZ@��P@�dZ@��R@��^@��@�j@��;@��@�S�@���@���@���@�M�@�J@�@���@��7@�/@��@�Ĝ@��D@�1'@��
@��F@��@���@��P@�S�@��y@��@���@�n�@�n�@�5?@�&�@��u@�r�@��@��9@��m@�dZ@�ȴ@�-@���@�r�@�I�@�1'@�1@���@�S�@��\@�n�@�n�@�ff@�M�@�5?@�J@�@�hs@��@��/@��@��u@�Z@�(�@��@��m@��F@��@�dZ@�\)@��@���@�33@��H@��+@�=q@�-@�-@�5?@�-@�-@��@�@��@���@��-@���@���@��7@�?}@���@��u@��@�Q�@�b@���@�@��R@��R@��R@��+@�^5@�V@�V@�V@�=q@�$�@�~�@��H@���@��\@��T@��@��@���@�V@�V@��@��@�j@��@�b@� �@��
@���@�t�@�S�@�K�@�+@�ȴ@��R@���@���@��\@�n�@�n�@�^5@�V@�=q@�5?@�$�@�@��@�@���@�X@�V@���@���@��u@�1@��F@��P@�l�@�;d@��@���@��!@�~�@�E�@��T@��^G�O�@���@|(�@rh
@i�.@c��@Z�@T�$@L<�@FV@?�@8?�@1%F@+�4@&�6@!�T@M@Y@J#@+@�611111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBXBXBXBXBXBXBXBXBXBYBYBYB\)B]/BgmBm�Bm�Bm�Bn�Bt�By�By�Bx�Bw�Br�By�B�B�oB��B��B��B�B�!B�wBƨB��B�#B�HB�NB�TB�TB�fB�fB��BBJBuB�B$�B/B=qBG�BM�BZBk�Bw�B�JB�1B}�B�'B��BbBoBVBDBB�#B�B�B�sB�B��BƨB�wB�FB�?B�B�B�B��BÖB�jBĜB�qBĜB�?B�-B��B� B]/BA�B49BuB�B�BŢB�jB�9B��B�oB|�Bm�B_;BO�BI�B<jB&�B�BBhBDB
��B
�B
�mB
�B
�}B
�!B
��B
}�B
e`B
6FB
JB	�B	�BB	��B	�qB	�B	��B	�DB	|�B	p�B	hsB	aHB	[#B	M�B	A�B	8RB	+B	�B	oB	JB	1B	B��B��B�B�B�mB�BB�#B�)B�BB�B��B��B��B��B��B�B�B�B�;B�/B�5B�HB�TB�NB�;B�/B�/B�;B�HB�TB�ZB�mB�B�B�B�B��B��B��B��B��B��B��B�B�B�B�NB�B��B��B��B��B��BŢB��B�}B�^B�RB�FB�9B�-B�-B�'B�'B�RB�dB�}BǮB��BƨB��BɺBƨB��B�wB�qB�}B�wB�?B�9BǮB��B��B��B��B��B��BǮBƨBĜB��B�jB�FB�B��B��B�B�9B�!B��B��B�B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�'B�-B�FB�^B�wB�}B�}B��B��B�}B��BƨBȴBȴB��B��B�
B�)B�BB�HB�`B�mB�B�B�B��B��B��B	B	B	B	B	1B	
=B	
=B	DB	hB	�B	�B	�B	�B	�B	"�B	&�B	&�B	)�B	/B	0!B	1'B	1'B	1'B	2-B	1'B	7LB	9XB	<jB	?}B	E�B	F�B	E�B	F�B	M�B	N�B	O�B	Q�B	S�B	VB	W
B	YB	^5B	cTB	dZB	ffB	hsB	jB	n�B	q�B	s�B	t�B	u�B	x�B	{�B	}�B	� B	�B	�B	�B	�B	�1B	�=B	�=B	�DB	�DB	�DB	�JB	�VB	�\B	�\B	�\B	�VB	�PB	�VB	�VB	�bB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�'B	�-B	�3B	�3B	�9B	�FB	�RB	�dB	�dB	�qB	�wB	��B	��B	ÖB	ŢB	ŢB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�#B	�#B	�)B	�)B	�/B	�;B	�BB	�BB	�HB	�NB	�NB	�NB	�NB	�NB	�TB	�ZB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
%B
%B
+B
+B
+B
1B
1B
1B
+B
	7B
	7B
	7B
	7B
	7B
	7B

=B
B
�B
(XB
-�B
5?B
;�B
B[B
J�B
S�B
YKB
^B
b�B
g�B
l�B
raB
uB
y	B
}qB
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111   BWBW BW BWBWBWBWBWBW BX%BX%BX#B[6B\=Bf{Bl�Bl�Bl�Bm�Bs�Bx�Bx�Bw�Bv�Bq�Bx�B�B�|B��B��B��B�B�-B�BŵB��B�.B�SB�VB�_B�_B�qB�tB��BBPB}B�B#�B.'B<}BF�BL�BY'Bj�Bv�B�VB�:B} B�3B��BnB|BaB
PBB�)B�&B�#B�|B�B��BŷB��B�RB�NB�B�B�B�B¥B�xBçB�|BèB�JB�>B��BB\7B@�B3KB�B�B�BĲB�zB�HB��B�B| Bl�B^JBN�BH�B;yB%�B�B+B{B
TB
��B
�B
�~B
�'B
��B
�.B
��B
}B
dtB
5ZB
]B	�B	�UB	��B	��B	�B	��B	�YB	|B	o�B	g�B	`]B	Z8B	L�B	@�B	7hB	*B	�B	�B	_B	HB	/B�B��B�B�B�B�ZB�8B�AB�^B�5B�B�B�B�B�B�B�,B�/B�PB�DB�JB�]B�kB�cB�QB�HB�FB�PB�`B�kB�qB�B�B�B��B��B��B��B��B��B��B��B��B��B�B�B�_B�)B��B��B��B��B��BĸB��B��B�xB�jB�^B�OB�CB�EB�<B�AB�kB�|B��B��B��B��B��B��B��B��B��B��B��B��B�UB�OB��B��B��B��B��B��B��B��B��BõB��B��B�]B�4B�B� B�!B�SB�9B�B�B�B��B��B��B��B��B��B��B��B��B�B�B� B�5B�2B�AB�GB�^B�wB��B��B��B��B��B��B��BſB��B��B��B�B�%B�BB�\B�bB�xB�B�B��B��B��B�B�B	 "B	%B	%B	2B	LB		TB		UB	
`B	�B	�B	�B	�B	�B	�B	!�B	&B	&B	)B	.4B	/:B	0?B	0DB	0BB	1HB	0>B	6fB	8qB	;�B	>�B	D�B	E�B	D�B	E�B	L�B	M�B	N�B	QB	SB	UB	V$B	X+B	]PB	blB	csB	e�B	g�B	i�B	m�B	p�B	r�B	s�B	t�B	w�B	{ B	}	B	B	�B	�B	�$B	�4B	�HB	�VB	�TB	�\B	�ZB	�^B	�dB	�oB	�qB	�sB	�tB	�lB	�eB	�lB	�kB	�xB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�B	�!B	�(B	�'B	�"B	�3B	�>B	�EB	�KB	�JB	�QB	�^B	�gB	�|B	�B	��B	��B	��B	��B	°B	ļB	ĺB	��B	��B	��B	��B	��B	��B	��B	��B	� B	�B	�B	�B	�B	�!B	�/B	�,B	�3B	�7B	�<B	�<B	�?B	�FB	�OB	�WB	�WB	�^B	�eB	�cB	�bB	�eB	�cB	�jB	�nB	�B	�B	�B	�B	�B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B
 B
&B
%B
(B
.B
0B
;B
5B
9B
@B
BB
BB
DB
DB
EB
HB
DB
NB
KB
KB
LB
JG�O�B
	SB
B
�B
'nB
,�B
4UB
:�B
AqB
I�B
R�B
XdB
]B
a�B
f�B
lB
qzB
tB
xB
|�B
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.09 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =1(+/-0.0001), vertically averaged dS =-0.001(+/-0.002) in PSS-78.                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Map scales: x=6,3; y=2,1. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                            201906040940212019060409402120190604094021  AO  ARCAADJP                                                                    20181121041150    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181121041150  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181121041150  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190604094021  IP                  G�O�G�O�G�O�                