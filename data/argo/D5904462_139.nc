CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-11-21T12:59:45Z creation      
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
_FillValue                    �,Argo profile    3.1 1.2 19500101000000  20181121125945  20190405100754  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  5287                            2C  D   APEX                            6529                            072314                          846 @��`)V��1   @��`���@0M�hr�!�d�1&�y1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   A   B   @���@�  A   A   A@  A`  A�  A�  A�  A�  A���A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dts3Dy�3D�3D�C3D�p D���D��D�9�D�p D��3D��D�I�D�y�D��fD�  D�33Dڌ�D��3D���D�@ D�D��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��A��A(��AH��Ah��A�z�A�z�A�z�A�z�A�G�A�z�A�z�A�z�B=qB
=qB=qB=qB"=qB*=qB2=qB:=qBB=qBJ=qBR=qBZ=qBb=qBj=qBr=qBz=qB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C �\C�\C�\C�\C�\C
�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C �\C"�\C$�\C&�\C(�\C*�\C,�\C.�\C0�\C2�\C4�\C6�\C8�\C:�\C<�\C>�\C@�\CB�\CD�\CF�\CH�\CJ�\CL�\CN�\CP�\CR�\CT�\CV�\CX�\CZ�\C\�\C^�\C`�\Cb�\Cd�\Cf�\Ch�\Cj�\Cl�\Cn�\Cp�\Cr�\Ct�\Cv�\Cx�\Cz�\C|�\C~�\C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�T{C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�D #�D ��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D	#�D	��D
#�D
��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D #�D ��D!#�D!��D"#�D"��D##�D#��D$#�D$��D%#�D%��D&#�D&��D'#�D'��D(#�D(��D)#�D)��D*#�D*��D+#�D+��D,#�D,��D-#�D-��D.#�D.��D/#�D/��D0#�D0��D1#�D1��D2#�D2��D3#�D3��D4#�D4��D5#�D5��D6#�D6��D7#�D7��D8#�D8��D9#�D9��D:#�D:��D;#�D;��D<#�D<��D=#�D=��D>#�D>��D?#�D?��D@#�D@��DA#�DA��DB#�DB��DC#�DC��DD#�DD��DE#�DE��DF#�DF��DG#�DG��DH#�DH��DI#�DI��DJ#�DJ��DK#�DK��DL#�DL��DM#�DM��DN#�DN��DO#�DO��DP#�DP��DQ#�DQ��DR#�DR��DS#�DS��DT#�DT��DU#�DU��DV#�DV��DW#�DW��DX#�DX��DY#�DY��DZ#�DZ��D[#�D[��D\#�D\��D]#�D]��D^#�D^��D_#�D_��D`#�D`��Da#�Da��Db#�Db��Dc#�Dc��Dd#�Dd��De#�De��Df#�Df��Dg#�Dg��Dh#�Dh��Di#�Di��Dj#�Dj��Dk#�Dk��Dl#�Dl��Dm#�Dm��Dn#�Dn��Do#�Do��Dp#�Dp��Dq#�Dq��Dr#�Dr��Ds#�Ds��Dt#�Dt�
Dy�
D�%D�UD���D���D�+�D�K�D���D��D��D�[�D���D��RD��D�EDڞ�D��D��D�Q�D�D��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�9XA�9XA�;dA�;dA�=qA�=qA�;dA�9XA�9XA�1'A�&�A��A�1A���A��A���A�A��A�p�A�l�A�hsA�dZA�`BA�^5A�\)A�XA�S�A�O�A�-A�oA�&�AہA�hsA��A��#Aڏ\A�M�A�oA���Aٕ�A��A�A�"�A־wA�|�A�/A���A�+A�x�A���A�M�Aџ�A��yA�
=A�%A���A·+A�bA͑hA�=qA�{AɸRA�dZAǑhA���A�l�AîA��TA�VA��A�oA�ȴA���A���A���A��A�1A���A�~�A��hA�^5A�p�A���A��A���A�A�A�ȴA�$�A��A�p�A���A�^5A���A���A�1A�9XA��`A��yA���A�A�C�A�I�A��TA�K�A��A��A��-A�$�A��HA���A�x�A���A�hsA��!A��+A��A�FA}XAz�Au��Ar$�Ao��Am�Al{Aj^5AhQ�Af�+Ac��A`VA\�A[hsAY��AX9XAT��AR �API�AJ��AGdZAE�FAC�mACVAA�PA?�mA>^5A<�DA:n�A9K�A8�A5�hA2��A2�A1|�A0�A/�A.jA-��A,�/A,9XA*r�A(E�A'A%�hA#�A#oA"�jA!��A ĜA ��A M�A ZA!�-A!&�A ��A A��A\)AffA&�A�TA�#A��A5?Al�A�A��An�A�AȴA�TA��A9XAA\)A�AƨAK�AA
ZA	��A	��A	%An�A�AAx�A%A��Az�A�
A��A�uAn�A�A�7AVA�RAr�A  At�A ��A ��A z�A ~�A �DA M�@���@�\)@��!@�n�@��@���@��u@��D@���@�(�@�\)@���@��@��@�j@�S�@���@�o@���@�J@�G�@�1'@�K�@�o@���@�R@��@�+@�5?@�O�@�I�@��m@�F@�@���@�E�@���@��@�~�@���@��@�@陚@�@���@�Q�@�ƨ@��
@�@�l�@�33@旍@�M�@���@�x�@��@�A�@�\@�V@��u@��@��
@�K�@�"�@��@އ+@�E�@�-@ݲ-@�r�@��
@ۅ@��@ڧ�@�^5@��T@���@��m@ו�@�@��@Ցh@�X@�&�@���@���@�S�@���@�5?@��@Ѻ^@��@��/@�9X@ύP@��@Η�@�n�@��@�?}@���@�A�@��
@˥�@�C�@�o@��H@�v�@�-@���@�p�@�&�@���@ț�@��m@ư!@�V@ř�@�/@�/@�&�@���@ċD@�1@�+@��@�ȴ@§�@�n�@��T@��7@�`B@�?}@��`@���@�S�@��@���@�E�@��T@��7@��@�Q�@��@��m@�|�@�@���@�V@�$�@�`B@�V@�Ĝ@�bN@�Z@�I�@��@��;@�ȴ@�~�@��\@��\@��+@�v�@�^5@�5?@�x�@�9X@���@�"�@�+@�;d@�C�@�K�@�+@��y@���@�$�@��-@���@�p�@�X@�&�@��@��@��D@�r�@� �@���@���@���@�ff@�J@�@��T@�hs@��@��D@�(�@��@��y@�5?@���@��h@�?}@��@�V@�%@���@��/@�z�@��@���@�t�@�l�@�dZ@��@���@���@�ff@�M�@���@��@�Z@���@��@���@�E�@�{@�@��7@�7L@��`@���@���@��@�Q�@� �@���@��m@��
@��F@���@�\)@��@���@��!@�M�@�{@�@��@��#@��^@��@��@�%@���@���@��D@�1'@��@�C�@�33@��@�O�@��@�ƨ@|�@t(�@j��@b��@[��@Q�^@I�7@>ff@7��@0Ĝ@,�D@&ȴ@!��@��@l�@��@�R11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�9XA�9XA�;dA�;dA�=qA�=qA�;dA�9XA�9XA�1'A�&�A��A�1A���A��A���A�A��A�p�A�l�A�hsA�dZA�`BA�^5A�\)A�XA�S�A�O�A�-A�oA�&�AہA�hsA��A��#Aڏ\A�M�A�oA���Aٕ�A��A�A�"�A־wA�|�A�/A���A�+A�x�A���A�M�Aџ�A��yA�
=A�%A���A·+A�bA͑hA�=qA�{AɸRA�dZAǑhA���A�l�AîA��TA�VA��A�oA�ȴA���A���A���A��A�1A���A�~�A��hA�^5A�p�A���A��A���A�A�A�ȴA�$�A��A�p�A���A�^5A���A���A�1A�9XA��`A��yA���A�A�C�A�I�A��TA�K�A��A��A��-A�$�A��HA���A�x�A���A�hsA��!A��+A��A�FA}XAz�Au��Ar$�Ao��Am�Al{Aj^5AhQ�Af�+Ac��A`VA\�A[hsAY��AX9XAT��AR �API�AJ��AGdZAE�FAC�mACVAA�PA?�mA>^5A<�DA:n�A9K�A8�A5�hA2��A2�A1|�A0�A/�A.jA-��A,�/A,9XA*r�A(E�A'A%�hA#�A#oA"�jA!��A ĜA ��A M�A ZA!�-A!&�A ��A A��A\)AffA&�A�TA�#A��A5?Al�A�A��An�A�AȴA�TA��A9XAA\)A�AƨAK�AA
ZA	��A	��A	%An�A�AAx�A%A��Az�A�
A��A�uAn�A�A�7AVA�RAr�A  At�A ��A ��A z�A ~�A �DA M�@���@�\)@��!@�n�@��@���@��u@��D@���@�(�@�\)@���@��@��@�j@�S�@���@�o@���@�J@�G�@�1'@�K�@�o@���@�R@��@�+@�5?@�O�@�I�@��m@�F@�@���@�E�@���@��@�~�@���@��@�@陚@�@���@�Q�@�ƨ@��
@�@�l�@�33@旍@�M�@���@�x�@��@�A�@�\@�V@��u@��@��
@�K�@�"�@��@އ+@�E�@�-@ݲ-@�r�@��
@ۅ@��@ڧ�@�^5@��T@���@��m@ו�@�@��@Ցh@�X@�&�@���@���@�S�@���@�5?@��@Ѻ^@��@��/@�9X@ύP@��@Η�@�n�@��@�?}@���@�A�@��
@˥�@�C�@�o@��H@�v�@�-@���@�p�@�&�@���@ț�@��m@ư!@�V@ř�@�/@�/@�&�@���@ċD@�1@�+@��@�ȴ@§�@�n�@��T@��7@�`B@�?}@��`@���@�S�@��@���@�E�@��T@��7@��@�Q�@��@��m@�|�@�@���@�V@�$�@�`B@�V@�Ĝ@�bN@�Z@�I�@��@��;@�ȴ@�~�@��\@��\@��+@�v�@�^5@�5?@�x�@�9X@���@�"�@�+@�;d@�C�@�K�@�+@��y@���@�$�@��-@���@�p�@�X@�&�@��@��@��D@�r�@� �@���@���@���@�ff@�J@�@��T@�hs@��@��D@�(�@��@��y@�5?@���@��h@�?}@��@�V@�%@���@��/@�z�@��@���@�t�@�l�@�dZ@��@���@���@�ff@�M�@���@��@�Z@���@��@���@�E�@�{@�@��7@�7L@��`@���@���@��@�Q�@� �@���@��m@��
@��F@���@�\)@��@���@��!@�M�@�{@�@��@��#@��^@��@��@�%@���@���@��D@�1'@��@�C�@�33@��@�O�@��@�ƨ@|�@t(�@j��@b��@[��@Q�^@I�7@>ff@7��@0Ĝ@,�D@&ȴ@!��@��@l�@��@�R11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�dB
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�7B
�B
�B
�B
�B
}�B
r�B
r�B
y�B
�DB
��B
��B
B
�B
��B
��B
��BB�BQ�BbNB`BB_;Bp�B�B�JB��B��BB�#B��B6FBH�B:^B:^BVBYBR�BP�BO�BO�BR�BYB^5BcTBbNBXBL�B5?B)�B%�B�BoB%B��B�B�sB�;B�
B��B�}B�B��Bz�B\)BF�B49B �B
��B
ŢB
��B
}�B
_;B
R�B
7LB
$�B
oB	��B	�B	��B	�-B	��B	��B	��B	�{B	�oB	�+B	t�B	hsB	bNB	\)B	T�B	E�B	8RB	,B	uB		7B	+B	B	B	B	  B	  B��B�B�B�fB�#B��B��B��B��B��BȴBȴBȴBƨB��B�jB�^B�RB�LB�^B�}BȴB��B�/B�`B��B	DB		7B	DB	1B�B�fB�sB�5B�B�`B�B�B�B�B�B�B��B��B�B�B�B�B�B�B��B��B��B��B��B��B	  B	B	1B	\B	hB	oB	�B	�B	�B	�B	!�B	#�B	)�B	.B	1'B	6FB	8RB	9XB	A�B	G�B	G�B	H�B	J�B	L�B	Q�B	[#B	]/B	^5B	aHB	e`B	iyB	s�B	x�B	}�B	�B	�B	�B	�B	�B	�B	�7B	�PB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�'B	�^B	��B	�}B	�}B	��B	ÖB	ĜB	ƨB	ǮB	ƨB	ŢB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�5B	�5B	�5B	�BB	�HB	�NB	�NB	�TB	�ZB	�`B	�fB	�fB	�fB	�fB	�mB	�sB	�sB	�sB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
  B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
B
B
B
B
%B
%B
%B
%B
%B
%B
%B
%B
%B
+B
+B
1B
1B
1B
1B
1B
1B
1B
1B
1B
1B
1B
1B
1B
1B
1B
1B
1B
1B
1B
+B
%B
B
%B
+B
+B
+B
1B
1B
	7B
	7B
	7B

=B
DB
JB
JB
JB
JB
DB
JB
JB
JB
JB
JB
JB
PB
PB
PB
PB
PB
VB
\B
bB
hB
hB
hB
hB
oB
uB
�B
VB
hB
�B
!�B
)�B
/B
9XB
?}B
D�B
J�B
N�B
YB
[#B
_;B
`BB
dZB
jB
n�B
s�B
v�B
z�11111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��G�O�B
��B
��B
��B
��B
��B
��B
��B
��B
ѿB
ѿB
��B
��B
��B
��B
нB
ϴB
˜B
�9B
��B
�{B
�uB
�mB
�hB
�UB
�zB
��B
��B
��B
�yB
�B
��B
��B
��B
��B
}�B
r�B
r�B
y�B
�B
�UB
��B
�bB
�lB
�B
��B
��B�B�BQ�BbB`B_BpvB��B�B�sB��B�cB��B��B6BH�B:/B:/BU�BX�BR�BP�BO�BO�BR�BX�B^Bc$BbBW�BL�B5B)�B%�B�B=B�B��B�iB�@B�B��B̚B�GB��B�pBz�B[�BFsB4 B �B
��B
�mB
��B
}�B
_B
R�B
7B
$�B
5B	��B	��B	�IB	��B	��B	��B	�_B	�?B	�6B	��B	t�B	h7B	bB	[�B	T�B	EdB	8B	+�B	8B	�B	�B	�B	�B	�B��B��B��B�kB�FB�'B��BԽBѭBЦB͔BʄB�sB�tB�uB�hB�IB�+B�B�B�B�B�;B�rBˆB��B�!B��B	B	�B	B	�B�DB�$B�1B��B��B�B�OB�CB�>B�GB�mB�tB��B�B�tB�hB�jB�kB�rB�tB��B��B��B��B��B��B��B	�B	�B	B	#B	+B	;B	NB	pB	oB	!�B	#�B	)�B	-�B	0�B	6B	8B	9B	ABB	GiB	GhB	HmB	J}B	L�B	Q�B	Z�B	\�B	]�B	aB	eB	i5B	srB	x�B	}�B	��B	��B	��B	��B	��B	��B	��B	�
B	�6B	�FB	�NB	�fB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�DB	�8B	�5B	�CB	�RB	�WB	�cB	�hB	�dB	�\B	�iB	ˁB	˂B	̆B	ΓB	ϙB	ϖB	ϚB	ϙB	РB	ѦB	ѦB	ӱB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�&B	�,B	�,B	�-B	�2B	�1B	�2B	�8B	�8B	�>B	�DB	�FB	�JB	�JB	�JB	�RB	�SB	�PB	�QB	�RB	�UB	�]B	�WB	�UB	�QB	�QB	�WB	�UB	�WB	�XB	�VB	�^B	�]B	�^B	�eB	�dB	�eB	�jB	�hB	�pB	�oB	�uB	�{B	�yB	�{B	�~B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
 �B	��B	��B	��B
 �B
 �B
�B
�B
�B
�B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	�B

�B
B
B
 B
 B

�B
B
 B
B
B
B
B
B
B
B
	B
B
B
B
B
"B
B
B
B
)B
+B
9B
B
B
EB
!�B
)�B
.�B
9B
?3B
DTB
JxB
N�B
X�B
Z�B
^�B
_�B
dB
j6B
nPB
snB
v�B
z�11111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.56 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             201904051007542019040510075420190405100754  AO  ARCAADJP                                                                    20181121125945    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181121125945  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181121125945  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190405100754  IP                  G�O�G�O�G�O�                