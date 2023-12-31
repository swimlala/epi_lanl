CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2017-08-04T00:01:15Z creation      
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
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20170804000115  20190405100806  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  5287                            2C  D   APEX                            6529                            072314                          846 @���H��1   @��b���@,�`A�7L�dz��`A�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   A   A   @@  @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�33B�33B�  B�  B���B���B�  B�  B���B���B���B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B���B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6�C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dtl�Dyl�D���D�VfD�vfD���D��3D�FfD��3D��3D�fD�9�D�i�D���D���D�9�D��D�� D�fD�I�D��D�ɚ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @c�
@��@��A��A(��AH��Ah��A�z�A�z�A�z�A�z�A�z�A�z�A�z�A�z�B=qB
=qB=qB=qB"=qB*=qB2=qB:=qBB=qBJ=qBR=qBZ=qBb=qBj=qBr=qBz=qB��B��B��B��B��B�Q�B�Q�B��B��B��B��B��B��B��RB��B��B��B��B��B��B��B�Q�B��B��B��B��B��B��B��B��B��B��C �\C�\C�\C�\C�\C
�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C �\C"�\C$�\C&�\C(�\C*�\C,�\C.�\C0�\C2�\C4�\C6��C8�\C:�\C<�\C>�\C@�\CB�\CD�\CF�\CH�\CJ�\CL�\CN�\CP�\CR�\CT�\CV�\CX�\CZ�\C\�\C^�\C`�\Cb�\Cd�\Cf�\Ch�\Cj�\Cl�\Cn�\Cp�\Cr�\Ct�\Cv�\Cx�\Cz�\C|�\C~�\C�G�C�G�C�G�C�G�C�G�C�G�C�T{C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�C�G�D #�D ��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D	#�D	��D
#�D
��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D��D#�D�=D#�D��D #�D ��D!#�D!��D"#�D"��D##�D#��D$#�D$��D%#�D%��D&#�D&��D'#�D'��D(#�D(��D)#�D)��D*#�D*��D+#�D+��D,#�D,��D-#�D-��D.#�D.��D/#�D/��D0#�D0��D1#�D1��D2#�D2��D3#�D3��D4#�D4��D5#�D5��D6#�D6��D7#�D7��D8#�D8��D9#�D9��D:#�D:��D;#�D;��D<#�D<��D=#�D=��D>#�D>��D?#�D?��D@#�D@��DA#�DA��DB#�DB��DC#�DC��DD#�DD��DE#�DE��DF#�DF��DG#�DG��DH#�DH��DI#�DI��DJ#�DJ��DK#�DK��DL#�DL��DM#�DM��DN#�DN��DO#�DO��DP#�DP��DQ#�DQ��DR#�DR��DS#�DS��DT#�DT��DU#�DU��DV#�DV��DW#�DW��DX#�DX��DY#�DY��DZ#�DZ��D[#�D[��D\#�D\��D]#�D]��D^#�D^��D_#�D_��D`#�D`��Da#�Da��Db#�Db��Dc#�Dc��Dd#�Dd��De#�De��Df#�Df��Dg#�Dg��Dh#�Dh��Di#�Di��Dj#�Dj��Dk#�Dk��Dl#�Dl��Dm#�Dm��Dn#�Dn��Do#�Do��Dp#�Dp��Dq#�Dq��Dr#�Dr��Ds#�Ds��Dt#�Dt��Dy��D��D�hRD��RD�޹D�D�XRD��D��D�RD�K�D�{�D�޹D��D�K�D�.�D���D�RD�[�D�D�ۆ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�9XA�C�A�7LA�1'A�1'A�/A�-A�-A�-A�-A�/A�-A�/A�-A� �A�"�A� �A��A��A��A�bA�VA�  A���A��;A���A�r�A�G�A�dZA�
=A��A��Aן�A�VA��A��HA֙�A�S�A�&�Aէ�Aӧ�A�/A���A��/A�t�AͮA�7LAʏ\A�l�Aǡ�A���A�
=A�&�A�%A�  A�"�A�K�A��^A��A�G�A���A�ȴA�5?A�K�A��A�&�A�1A�-A���A��A���A���A���A�p�A�5?A���A�~�A���A��wA��9A�A��A�ƨA��A�Q�A�ȴA�E�A�(�A���A�$�A���A��PA�VA���A�p�A��yA�ȴA�{A��yA��!A~�yA}l�Azv�Au�Aq�Am�PAkoAgp�Aa�
A]�#AZ��AY;dAX  AU��AS��AR��AQ�TAQ7LAO��AN�AM��AL�AI�hAF��AD�A@�!A=?}A:�jA:��A:�+A9�PA6�/A5��A5G�A4��A4��A3;dA1�A/G�A-�7A-33A,�9A,I�A, �A+�mA+`BA*�HA*VA)K�A(VA&��A%�wA%A$ȴA$VA#��A#/A"�A!%A�PA�jAƨA7LAȴA-A�mAƨA�An�A;dA��A�AdZAoAS�AO�A$�A1'AȴAZA\)AVAO�A��AM�A�hA�+AffAI�A=qAA�-Al�A
^5A	ƨA	��A	�A	��A	�7A	XAZA�FA��A��A�7A9XA+AȴAn�A�A��A �A ��A �DA ~�A z�A �DA r�A �@��@��@��j@��P@��!@�V@��@�9X@��
@��\@���@�+@�@�1@�"�@���@�!@��@�M�@���@��#@�h@�V@�bN@�b@�@��@�/@�1@���@�  @���@�@��y@�h@��@�7@�z�@�1@睲@�ȴ@�^@�G�@�I�@�$�@�G�@��/@�I�@��m@���@���@��
@�
=@�-@��@��`@���@ܓu@ۥ�@�C�@�-@ٺ^@ٙ�@�p�@ّh@���@���@ؓu@�dZ@�ȴ@�5?@�E�@�$�@��@�Z@�+@�-@љ�@��@Ѓ@�K�@�+@�@�@�o@���@���@���@·+@�J@͙�@�`B@�V@�Z@��@˕�@�;d@���@�v�@��@���@�$�@ɉ7@ȣ�@�A�@ǝ�@�$�@��@���@ě�@�j@�Z@�bN@ă@�Ĝ@�Ĝ@ě�@ċD@�Ĝ@�Ĝ@�Q�@��@��m@�t�@�S�@���@¸R@\@�~�@�5?@�@��-@�p�@�O�@�V@��j@�I�@�ƨ@�t�@�K�@�;d@�+@��@�
=@���@��H@��H@�v�@���@�hs@���@��@�b@�ƨ@�;d@�
=@��R@�M�@��@���@�p�@���@��@�ƨ@�;d@�
=@��\@��@��@��@�Ĝ@�bN@�1'@���@�C�@�@�ȴ@��\@��@���@���@�z�@�Z@�A�@� �@���@�dZ@�33@���@���@�M�@��@��h@�G�@�Z@��m@��w@�t�@�@���@�-@��h@�x�@��7@�/@���@�z�@�1'@��m@�l�@�@�$�@��-@���@�9X@�  @���@���@�|�@�t�@�l�@��\@�M�@�{@��#@��@���@��u@�I�@�b@��m@�|�@�+@�"�@��@��@��!@�ff@�J@��^@��@�`B@�%@��@�1'@���@�;d@���@���@�v�@�V@��T@�?}@�7L@���@�z�@�bN@�A�@��m@��@�@��R@�ff@�5?@��#@�@���@��@���@�r�@�{@���@�v�@�O�@y�^@r=q@i��@`��@X  @Nff@G�@?��@8A�@1�@*��@'K�@#C�@l�@�#@�/@ �111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�9XA�C�A�7LA�1'A�1'A�/A�-A�-A�-A�-A�/A�-A�/A�-A� �A�"�A� �A��A��A��A�bA�VA�  A���A��;A���A�r�A�G�A�dZA�
=A��A��Aן�A�VA��A��HA֙�A�S�A�&�Aէ�Aӧ�A�/A���A��/A�t�AͮA�7LAʏ\A�l�Aǡ�A���A�
=A�&�A�%A�  A�"�A�K�A��^A��A�G�A���A�ȴA�5?A�K�A��A�&�A�1A�-A���A��A���A���A���A�p�A�5?A���A�~�A���A��wA��9A�A��A�ƨA��A�Q�A�ȴA�E�A�(�A���A�$�A���A��PA�VA���A�p�A��yA�ȴA�{A��yA��!A~�yA}l�Azv�Au�Aq�Am�PAkoAgp�Aa�
A]�#AZ��AY;dAX  AU��AS��AR��AQ�TAQ7LAO��AN�AM��AL�AI�hAF��AD�A@�!A=?}A:�jA:��A:�+A9�PA6�/A5��A5G�A4��A4��A3;dA1�A/G�A-�7A-33A,�9A,I�A, �A+�mA+`BA*�HA*VA)K�A(VA&��A%�wA%A$ȴA$VA#��A#/A"�A!%A�PA�jAƨA7LAȴA-A�mAƨA�An�A;dA��A�AdZAoAS�AO�A$�A1'AȴAZA\)AVAO�A��AM�A�hA�+AffAI�A=qAA�-Al�A
^5A	ƨA	��A	�A	��A	�7A	XAZA�FA��A��A�7A9XA+AȴAn�A�A��A �A ��A �DA ~�A z�A �DA r�A �@��@��@��j@��P@��!@�V@��@�9X@��
@��\@���@�+@�@�1@�"�@���@�!@��@�M�@���@��#@�h@�V@�bN@�b@�@��@�/@�1@���@�  @���@�@��y@�h@��@�7@�z�@�1@睲@�ȴ@�^@�G�@�I�@�$�@�G�@��/@�I�@��m@���@���@��
@�
=@�-@��@��`@���@ܓu@ۥ�@�C�@�-@ٺ^@ٙ�@�p�@ّh@���@���@ؓu@�dZ@�ȴ@�5?@�E�@�$�@��@�Z@�+@�-@љ�@��@Ѓ@�K�@�+@�@�@�o@���@���@���@·+@�J@͙�@�`B@�V@�Z@��@˕�@�;d@���@�v�@��@���@�$�@ɉ7@ȣ�@�A�@ǝ�@�$�@��@���@ě�@�j@�Z@�bN@ă@�Ĝ@�Ĝ@ě�@ċD@�Ĝ@�Ĝ@�Q�@��@��m@�t�@�S�@���@¸R@\@�~�@�5?@�@��-@�p�@�O�@�V@��j@�I�@�ƨ@�t�@�K�@�;d@�+@��@�
=@���@��H@��H@�v�@���@�hs@���@��@�b@�ƨ@�;d@�
=@��R@�M�@��@���@�p�@���@��@�ƨ@�;d@�
=@��\@��@��@��@�Ĝ@�bN@�1'@���@�C�@�@�ȴ@��\@��@���@���@�z�@�Z@�A�@� �@���@�dZ@�33@���@���@�M�@��@��h@�G�@�Z@��m@��w@�t�@�@���@�-@��h@�x�@��7@�/@���@�z�@�1'@��m@�l�@�@�$�@��-@���@�9X@�  @���@���@�|�@�t�@�l�@��\@�M�@�{@��#@��@���@��u@�I�@�b@��m@�|�@�+@�"�@��@��@��!@�ff@�J@��^@��@�`B@�%@��@�1'@���@�;d@���@���@�v�@�V@��T@�?}@�7L@���@�z�@�bN@�A�@��m@��@�@��R@�ff@�5?@��#@�@���@��@���@�r�@�{@���@�v�@�O�@y�^@r=q@i��@`��@X  @Nff@G�@?��@8A�@1�@*��@'K�@#C�@l�@�#@�/@ �111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�+B	�+B	�+B	�1B	�7B	�7B	�=B	�=B	�JB	�JB	�PB	�PB	�VB	��B	�?B	�LB	�FB	�?B	�3B	�-B	�-B	�3B	�3B	�'B	�-B	�FB	�B
B
�B
/B
33B
>wB
]/B
�B
�?B
�LB
�B �B<jB~�B��B�B�}B��B�B�ZB�B��B	7B�B�B �B�B#�B&�B+BbB��B��BuB�B{B
=B��B�#B�}B�B}�BP�B#�B1B
��B
�B
�B
ŢB
�jB
ƨB
�B
�oB
|�B
J�B
1'B
�B	��B	�B	��B	�FB	�B	��B	�B	r�B	e`B	ZB	F�B	5?B	%�B	�B	�B	�B	bB	PB	DB	
=B		7B	
=B	
=B	1B	B	B��B��B	B	1B	uB	oB	hB	{B	%�B	1'B	2-B	2-B	0!B	8RB	@�B	M�B	[#B	^5B	aHB	dZB	e`B	ffB	iyB	l�B	n�B	r�B	t�B	y�B	{�B	}�B	~�B	� B	� B	~�B	{�B	z�B	y�B	x�B	y�B	y�B	y�B	{�B	|�B	|�B	|�B	z�B	y�B	t�B	t�B	z�B	�B	{�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	�B	�LB	�RB	�LB	�?B	�!B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�-B	�-B	�3B	�!B	�!B	�'B	�B	�B	�B	�B	�!B	�-B	�B	�B	�9B	�jB	�wB	ÖB	ĜB	ƨB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	ɺB	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	��B	��B	��B	��B	��B	ȴB	ĜB	ÖB	��B	B	ÖB	��B	��B	��B	��B	��B	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�B	�5B	�;B	�#B	�B	�
B	�
B	�B	�B	�
B	�B	�B	�)B	�;B	�ZB	�mB	�mB	�mB	�mB	�sB	�B	�B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
  B
  B	��B	��B	��B
  B
  B
  B
  B
B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
+B
+B
+B
+B
+B
+B
1B
1B
1B
	7B
	7B
	7B
	7B
	7B
	7B
1B
1B
+B
%B
%B
%B
%B
1B
1B
+B
+B
	7B

=B

=B
	7B
1B
+B
%B
B
%B
%B
%B
+B
+B
%B
1B
1B
1B
1B
+B
+B
1B
1B
1B
1B
	7B

=B

=B
DB
JB
VB
\B
bB
hB
hB
hB
hB
oB
oB
oB
oB
oB
oB
oB
oB
oB
{B
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
"�B
,B
5?B
;dB
A�B
D�B
K�B
P�B
W
B
ZB
_;B
aHB
gmB
k�B
m�B
q�B
t�B
t�B
w�B
|�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�	B	�
B	�
B	�B	�B	�B	�B	�$B	�%B	�+B	��B	�B	� B	�B	�B	�B	�B	�B	�B	�B	��B	�B	�B	��B
 �B
�B
.�B
3B
>JB
]B
��B
�B
�!B
��B �B<=B~�B�[B��B�KB˕B��B�+B�{B��B	BXB�B �B�B#�B&�B*�B1B��B��BBBjBJB

B��B��B�IB��B}�BP�B#�B�B
��B
�uB
��B
�kB
�6B
�rB
��B
�7B
|�B
J�B
0�B
[B	��B	�`B	ϪB	�B	��B	�SB	��B	rtB	e%B	Y�B	FnB	5B	%�B	jB	OB	FB	&B	B	
B	
B	�B		�B		�B	�B	�B	 �B��B��B	 �B	�B	7B	1B	'B	>B	%�B	0�B	1�B	1�B	/�B	8B	@CB	M�B	Z�B	]�B	aB	dB	eB	f'B	i7B	lJB	nXB	rnB	tzB	y�B	{�B	}�B	~�B	�B	�B	~�B	{�B	z�B	y�B	x�B	y�B	y�B	y�B	{�B	|�B	|�B	|�B	z�B	y�B	tzB	tzB	z�B	��B	{�B	��B	�]B	��B	��B	��B	��B	��B	��B	�hB	�cB	�[B	�]B	�iB	�hB	�oB	��B	��B	��B	��B	��B	��B	�	B	�B	�
B	��B	��B	��B	��B	��B	��B	�nB	�PB	�IB	�GB	�[B	�tB	�fB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�"B	�1B	�QB	�WB	�fB	�jB	�nB	˂B	˂B	ϗB	ОB	НB	ПB	ϗB	ˁB	�uB	̉B	ΔB	ϚB	ҭB	ѥB	ӳB	��B	��B	սB	սB	ӱB	ѨB	ӲB	ҬB	ϙB	�mB	�WB	�OB	�CB	�HB	�NB	�|B	̄B	͍B	̇B	�{B	�rB	�tB	�uB	˂B	ΕB	ΒB	ΐB	ΓB	ΔB	ҬB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�&B	�&B	�'B	�'B	�,B	�8B	�6B	�1B	�7B	�8B	�4B	�5B	�?B	�?B	�>B	�NB	�]B	�UB	�IB	�BB	�DB	�>B	�?B	�>B	�>B	�CB	�AB	�IB	�WB	�eB	�iB	�gB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B	��B	��B	��B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	�B
	�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	�B
	�B

�B
B
B
B
B
 B
!B
!B
 B
'B
%B
$B
(B
%B
(B
&B
%B
&B
2B
-B
1B
9B
:B
7B
8B
:B
9B
8B
<B
=B
=B
<B
>B
CB
JB
[B
eB
"�B
+�B
4�B
;B
AAB
DSB
K}B
P�B
V�B
Y�B
^�B
a B
g%B
k>B
mIB
qcB
trB
tuB
w�B
|�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.56 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             201904051008062019040510080620190405100806  AO  ARCAADJP                                                                    20170804000115    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20170804000115  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20170804000115  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190405100806  IP                  G�O�G�O�G�O�                