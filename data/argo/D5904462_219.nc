CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2017-10-11T17:02:45Z creation      
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
resolution        :�o     �  q�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    y�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �|   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �|   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �t   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �    HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �0   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �4   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �D   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �H   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �L   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20171011170245  20190405100808  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  5287                            2C  D   APEX                            6529                            072314                          846 @�,ևeND1   @�,�-��@-/��w�d�333331   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�33A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD�CF  CH  CJ  CL  CN  CP  CQ�fCT  CV  CX  CZ  C\�C^�C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct�Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX�fDY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�fDy��D��D�@ D���D�ɚD�  D�33D�|�D�� D��D�@ D���D���D�fD�P DچfD���D�3D�,�D�ffD���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��\@ҏ\A	G�A)G�AIG�AiG�A���A���A���A���Aģ�A��
A��A���BQ�B
Q�BQ�BQ�B"Q�B*Q�B2Q�B:Q�BBQ�BJQ�BRQ�BZQ�BbQ�BjQ�BrQ�BzQ�B�(�B�(�B�(�B�\)B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�\)B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�\)B�(�B�(�B�(�B�(�B�(�B�(�C �{C�{C�{C�{C�{C
�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C �{C"�{C$�{C&�{C(�{C*�{C,�{C.�{C0�{C2�{C4�{C6�{C8�{C:�{C<�{C>�{C@�{CB�{CD�CF�{CH�{CJ�{CL�{CN�{CP�{CRz�CT�{CV�{CX�{CZ�{C\�C^�C`�{Cb�{Cd�{Cf�{Ch�{Cj�{Cl�{Cn�{Cp�{Cr�{Ct�Cv�{Cx�{Cz�{C|�{C~�{C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=C�J=D %D �D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D	%D	�D
%D
�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D%D�D %D �D!%D!�D"%D"�D#%D#�D$%D$�D%%D%�D&%D&�D'%D'�D(%D(�D)%D)�D*%D*�D+%D+�D,%D,�D-%D-�D.%D.�D/%D/�D0%D0�D1%D1�D2%D2�D3%D3�D4%D4�D5%D5�D6%D6�D7%D7�D8%D8�D9%D9�D:%D:�D;%D;�D<%D<�D=%D=�D>%D>�D?%D?�D@%D@�DA%DA�DB%DB�DC%DC�DD%DD�DE%DE�DF%DF�DG%DG�DH%DH�DI%DI�DJ%DJ�DK%DK�DL%DL�DM%DM�DN%DN�DO%DO�DP%DP�DQ%DQ�DR%DR�DS%DS�DT%DT�DU%DU�DV%DV�DW%DW�DX%DX��DY%DY�DZ%DZ�D[%D[�D\%D\�D]%D]�D^%D^�D_%D_�D`%D`�Da%Da�Db%Db�Dc%Dc�Dd%Dd�De%De�Df%Df�Dg%Dg�Dh%Dh�Di%Di�Dj%Dj�Dk%Dk�Dl%Dl�Dm%Dm�Dn%Dn�Do%Do�Dp%Dp�Dq%Dq�Dr%Dr�Ds%Ds�Dt%Dt�Du�Dy��D�\D�R�D��)D��)D��D�E�D��\D�D�\D�R�D��\D��\D��D�b�Dژ�D��\D��D�?\D�x�D��\111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A� �A��A�"�A�"�A�&�A�(�A�(�A�+A�-A�-A�-A�+A�&�A�&�A�&�A�-A�+A�+A�+A�+A��A�bA�bA���A��A��A��A��A��`A���A�!A��A���Aٗ�A��HA։7A�l�A�O�AӁA�M�A�7LA�bNAϝ�A�t�A��yA�;dA��A˥�A��mA�Q�A�`BA�K�A�G�Aţ�AĶFA�"�A�ZA���A��A�1'A��A�~�A���A�1'A��A���A��RA�Q�A��yA�O�A���A��A��mA��!A���A��A��A��FA�^5A�=qA��A�M�A���A��RA�1'A��A�p�A�A�A�1A��mA�Q�A��jA�9XA�/A���A���A���A��TA���A�z�A��7A���A�(�A�dZA�ƨA��A�VA�n�A{%Ay��Ax��Av5?Ai+A^��AZ^5AX�\AS�AO��AM��AL�!AKS�AG�
AD��AC��A>z�A<VA<{A;��A;�A:v�A9�mA9G�A8�jA6�+A5A4��A4I�A3��A4�A4ZA4�A4��A4��A4�DA4~�A3��A3/A4A4�A4�A4��A4(�A2��A1�TA1��A0ffA.�\A.  A-+A,(�A+XA*�`A)��A(�jA'��A&��A&5?A%��A%��A%l�A%oA$�A$I�A#�A#|�A#�A"�A!�;A!�A �+A n�A VA �AdZA�DAE�A��AXAr�A��AK�A+A��A=qA`BAĜA-A�TA�wA��Ax�A`BA�AȴA�+AZA9XAA��AoA^5A��A��Al�A�RAƨAx�A�A�mA�PAO�A33A&�A�AVAA�AȴA~�A�;A+A�RAVA�A�mA\)A
�uA
A�A	�FA	S�A	O�A	?}A	33A	"�A	&�A	&�A	"�A	oA��A��A��A�A��A�AA��A?}A9XA{A��A��A�FA��AXA�A�#At�A33A��A�AZA��A �jA ��A v�A Z@��
@��#@���@�%@���@�r�@�b@��w@�l�@�33@�"�@��y@�M�@���@�V@���@��@�
=@�ff@��@���@��@��
@��H@��@��@��T@���@�@�X@�Z@��
@��@�S�@��H@@�V@��#@웦@�9X@��@�F@�;d@��@�\@�=q@�@���@�hs@��@�z�@��@�C�@�bN@�@�n�@�@��#@�/@�j@߶F@�+@�
=@ޏ\@��@�J@�%@؃@؛�@؋D@�I�@׾w@ְ!@�=q@���@��@�-@�J@���@�?}@�j@�  @���@���@ӝ�@�dZ@�
=@��T@�?}@Ь@�1@�S�@�
=@��@�A�@ʰ!@ʏ\@�~�@�^5@�-@�$�@�@��@ə�@�G�@��`@�Z@�  @ǝ�@��@Ɨ�@���@���@Ĵ9@�b@�K�@�v�@�j@��
@��R@�n�@��@�x�@��@���@�z�@�Z@�A�@�9X@� �@�  @��m@�\)@��@�%@���@��D@�C�@���@�x�@�V@��@�(�@��m@��w@�l�@�o@�E�@���@��@��@��T@���@�O�@�%@��@��j@��D@�r�@�Q�@� �@�dZ@�ȴ@��@�Q�@�S�@��7@�  @���@�C�@�@���@���@��!@��!@���@���@�~�@�v�@�ff@�M�@�=q@��@�@��@���@�x�@�G�@��@��u@�C�@��R@���@��\@�n�@��@��@�?}@��@��@�Ĝ@��D@��@��P@���@�K�@�ȴ@���@�v�@�5?@�@��h@��@��@���@��^@�9X@}`B@vV@l�@fE�@]O�@K��@BJ@=�@5��@1%@*��@&ȴ@!hs@V@��@;d@o111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A� �A��A�"�A�"�A�&�A�(�A�(�A�+A�-A�-A�-A�+A�&�A�&�A�&�A�-A�+A�+A�+A�+A��A�bA�bA���A��A��A��A��A��`A���A�!A��A���Aٗ�A��HA։7A�l�A�O�AӁA�M�A�7LA�bNAϝ�A�t�A��yA�;dA��A˥�A��mA�Q�A�`BA�K�A�G�Aţ�AĶFA�"�A�ZA���A��A�1'A��A�~�A���A�1'A��A���A��RA�Q�A��yA�O�A���A��A��mA��!A���A��A��A��FA�^5A�=qA��A�M�A���A��RA�1'A��A�p�A�A�A�1A��mA�Q�A��jA�9XA�/A���A���A���A��TA���A�z�A��7A���A�(�A�dZA�ƨA��A�VA�n�A{%Ay��Ax��Av5?Ai+A^��AZ^5AX�\AS�AO��AM��AL�!AKS�AG�
AD��AC��A>z�A<VA<{A;��A;�A:v�A9�mA9G�A8�jA6�+A5A4��A4I�A3��A4�A4ZA4�A4��A4��A4�DA4~�A3��A3/A4A4�A4�A4��A4(�A2��A1�TA1��A0ffA.�\A.  A-+A,(�A+XA*�`A)��A(�jA'��A&��A&5?A%��A%��A%l�A%oA$�A$I�A#�A#|�A#�A"�A!�;A!�A �+A n�A VA �AdZA�DAE�A��AXAr�A��AK�A+A��A=qA`BAĜA-A�TA�wA��Ax�A`BA�AȴA�+AZA9XAA��AoA^5A��A��Al�A�RAƨAx�A�A�mA�PAO�A33A&�A�AVAA�AȴA~�A�;A+A�RAVA�A�mA\)A
�uA
A�A	�FA	S�A	O�A	?}A	33A	"�A	&�A	&�A	"�A	oA��A��A��A�A��A�AA��A?}A9XA{A��A��A�FA��AXA�A�#At�A33A��A�AZA��A �jA ��A v�A Z@��
@��#@���@�%@���@�r�@�b@��w@�l�@�33@�"�@��y@�M�@���@�V@���@��@�
=@�ff@��@���@��@��
@��H@��@��@��T@���@�@�X@�Z@��
@��@�S�@��H@@�V@��#@웦@�9X@��@�F@�;d@��@�\@�=q@�@���@�hs@��@�z�@��@�C�@�bN@�@�n�@�@��#@�/@�j@߶F@�+@�
=@ޏ\@��@�J@�%@؃@؛�@؋D@�I�@׾w@ְ!@�=q@���@��@�-@�J@���@�?}@�j@�  @���@���@ӝ�@�dZ@�
=@��T@�?}@Ь@�1@�S�@�
=@��@�A�@ʰ!@ʏ\@�~�@�^5@�-@�$�@�@��@ə�@�G�@��`@�Z@�  @ǝ�@��@Ɨ�@���@���@Ĵ9@�b@�K�@�v�@�j@��
@��R@�n�@��@�x�@��@���@�z�@�Z@�A�@�9X@� �@�  @��m@�\)@��@�%@���@��D@�C�@���@�x�@�V@��@�(�@��m@��w@�l�@�o@�E�@���@��@��@��T@���@�O�@�%@��@��j@��D@�r�@�Q�@� �@�dZ@�ȴ@��@�Q�@�S�@��7@�  @���@�C�@�@���@���@��!@��!@���@���@�~�@�v�@�ff@�M�@�=q@��@�@��@���@�x�@�G�@��@��u@�C�@��R@���@��\@�n�@��@��@�?}@��@��@�Ĝ@��D@��@��P@���@�K�@�ȴ@���@�v�@�5?@�@��hG�O�@��@���@��^@�9X@}`B@vV@l�@fE�@]O�@K��@BJ@=�@5��@1%@*��@&ȴ@!hs@V@��@;d@o111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
W
B
W
B
W
B
W
B
W
B
W
B
W
B
W
B
W
B
W
B
W
B
W
B
W
B
VB
W
B
W
B
W
B
W
B
VB
VB
VB
T�B
T�B
T�B
S�B
S�B
S�B
S�B
R�B
O�B
K�B
>wB
{B	��B	�RB	�B	�B	�dB	�ZB	��B	��B
1B
,B
:^B
e`B
��B
��B
��B
�-B
�#B
�B\B%�B2-BD�BdZBt�B�DB��B��B��B��B��BǮBȴB��B��B�B�B��BBPB\B �B'�B-B1'B1'B2-B?}BD�BE�BF�BE�BD�BB�B7LB+B�BoB  B�B��BB�'B��B��B�%B^5B;dB�B%B
��B
��B
�{B
7LB
�B	�fB	�qB	�3B	�B	��B	P�B	'�B	�B	oB	DB	1B		7B		7B	1B	JB	oB	uB	!�B	%�B	$�B	%�B	)�B	2-B	8RB	B�B	N�B	�VB	��B	ÖB	�yB	��B
	7B
"�B
=qB
B�B
F�B
J�B
O�B
YB
\)B
{�B
�%B
�oB
�{B
�{B
��B
��B
�{B
�DB
� B
|�B
v�B
o�B
n�B
n�B
hsB
bNB
\)B
W
B
S�B
Q�B
P�B
O�B
M�B
K�B
I�B
H�B
F�B
D�B
C�B
A�B
?}B
>wB
>wB
>wB
=qB
=qB
<jB
:^B
:^B
8RB
7LB
7LB
7LB
6FB
6FB
5?B
5?B
6FB
8RB
9XB
8RB
9XB
8RB
8RB
9XB
<jB
=qB
>wB
=qB
=qB
>wB
=qB
=qB
>wB
?}B
>wB
=qB
;dB
:^B
;dB
;dB
:^B
9XB
9XB
9XB
9XB
9XB
8RB
8RB
6FB
5?B
5?B
6FB
6FB
6FB
8RB
7LB
5?B
1'B
/B
-B
+B
+B
+B
+B
,B
-B
-B
.B
.B
.B
.B
.B
-B
,B
+B
+B
)�B
'�B
&�B
&�B
%�B
%�B
$�B
$�B
$�B
!�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
�B
�B
�B
�B
{B
uB
uB
oB
oB
oB
oB
oB
hB
oB
oB
oB
hB
hB
hB
hB
bB
hB
hB
hB
hB
hB
bB
bB
bB
\B
\B
VB
VB
PB
JB

=B
%B
%B
B
B
B
B
B
B
B
B
B
  B	��B	��B
  B
B
+B
%B
B
B
B
B
+B
1B
1B
1B
1B
	7B
	7B
	7B
	7B
	7B
1B
1B
1B
+B
+B
%B
%B
B
B
B
B
B
B
B
B
B
B
B
B
B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
+B
1B
	7B

=B
DB
DB
DB
DB
JB
JB
JB
DB
DB
DB

=B
DB
JB
PB
PB
PB
PB
PB
VB
VB
VB
VB
VB
VB
VB
VB
VB
VB
VB
VB
VB
VB
VB
VB
VB
bB
bB
bB
bB
bB
hB
oB
uB
uB
{B
{B
{B
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
$�B
-B
49B
8RB
;dB
?}B
C�B
H�B
P�B
W
B
YB
]/B
`BB
hsB
n�B
s�B
t�B
x�B
{�B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B
V�B
V�B
V�B
V�B
V�B
V�B
V�B
V�B
V�B
V�B
V�B
V�B
V�B
U�B
V�B
V�B
V�B
V�B
U�B
U�B
U�B
T�B
T�B
T�B
S�B
S�B
S�B
S�B
R�B
O�B
K�B
>IB
PB	ϱB	�$B	��B	��B	�7B	�)B	�B	�B
B
+�B
:.B
e3B
�cB
��B
��B
� B
��B
�B-B%�B1�BDmBd)Bt�B�B�PB��B��B��B�QB�~BȁB˓BдB��B�`B��B�BB(B �B'�B,�B0�B0�B1�B?KBDiBEoBFsBElBDhBB\B7B*�B�B9B��B�[BбB�[B��B��B��B��B]�B;.BnB�B
��B
��B
�BB
7B
IB	�+B	�4B	��B	��B	�FB	P�B	'�B	aB	2B	B	�B	�B	�B	�B	B	1B	6B	!�B	%�B	$�B	%�B	)�B	1�B	8B	BNB	N�B	�B	��B	�SB	�:B	��B
�B
"�B
=1B
BLB
FeB
J}B
O�B
X�B
[�B
{�B
��B
�.B
�6B
�:B
�iB
�KB
�8B
��B
�B
|�B
v�B
o\B
nVB
nVB
h0B
bB
[�B
V�B
S�B
Q�B
P�B
O�B
M�B
K�B
ItB
HoB
FdB
DVB
CSB
ADB
?6B
>4B
>0B
>5B
=,B
=+B
<&B
:B
:B
8B
7B
7B
7B
6B
6 B
4�B
4�B
6 B
8B
9B
8B
9B
8B
8B
9B
<'B
=.B
>1B
=,B
=+B
>2B
=,B
=.B
>2B
?4B
>0B
=)B
; B
:B
;B
;!B
:B
9B
9B
9B
9B
9B
8
B
8B
6B
4�B
4�B
6B
5�B
6B
8B
7B
4�B
0�B
.�B
,�B
*�B
*�B
*�B
*�B
+�B
,�B
,�B
-�B
-�B
-�B
-�B
-�B
,�B
+�B
*�B
*�B
)�B
'�B
&�B
&�B
%�B
%�B
$�B
$�B
$�B
!�B
xB
qB
sB
lB
eB
^B
]B
[B
YB
UB
NB
IB
FB
>B
@B
AB
@B
AB
@B
?B
@B
BB
:B
:B
:B
>B
:B
4B
9B
9B
9B
:B
6B
-B
0B
)B
(B
(B
*B
)B
#B
(B
'B
*B
B
 B
B
 B
B
B
"B
"B
"B
B
B
B
B
B
B
B
B
B
B
	�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B	��B	��B	��B	��B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�yB	�nB	�bB	�VB	�`B	�`B	�aB	�aB	�gB	�gB	�fB	�gB	�nB	�fB	�fB	�fB	�fB	�eB	�rB	�rB	�rB	�tB	�zB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
�B
�B
�B
�B
	�B

�B

�B

�B

�B
B
B
 B

�B

�B

�B
	�B

�B
 B
B
B
B
B
B

B
B
B

B

B
B
B
B
B
B
B
B
B
B
B

B
	B
B
B
B
B
B
B
%B
)B
)B
0B
.B
.B
)B
1B
AB
NB
GB
IB
CB
<B
<B
AG�O�B
PB
$�B
,�B
3�B
8B
;B
?2B
CJB
HhB
P�B
V�B
X�B
\�B
_�B
h)B
nNB
skB
tpB
x�B
{�B
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.58 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             201904051008082019040510080820190405100808  AO  ARCAADJP                                                                    20171011170245    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20171011170245  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20171011170245  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190405100808  IP                  G�O�G�O�G�O�                