CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-11-21T12:59:48Z creation      
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
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20181121125948  20190405100756  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  5287                            2C  D   APEX                            6529                            072314                          846 @�Ԅ�Hrq1   @�ԅ\�&�@0)�^5?}�dk��Q�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A���A�  A�  A�  B   B  B  BffB   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx��B��B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C�C�C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI�fDJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt��Dy�3D��D�FfD�y�D���D�	�D�L�D�p D�� D�  D�P D�� D�� D���D�@ Dړ3D�� D���D�33D�l�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @���@���Az�A(z�AHz�Ahz�A�=qA�=qA�=qA�=qA�
>A�=qA�=qA�=qB�B
�B�B�B"�B*�B2�B:�BB�BJ�BR�BZ�Bb�Bj�Br�Bz�B��)B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B��)B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\C ��C��C�HC�HC��C
��C��C��C��C��C��C��C��C��C��C��C ��C"��C$��C&��C(��C*��C,��C.��C0��C2��C4��C6��C8��C:��C<��C>��C@��CB��CD��CF��CH��CJ��CL��CN��CP��CR��CT��CV��CX��CZ��C\��C^��C`��Cb��Cd��Cf��Ch��Cj��Cl��Cn��Cp��Cr��Ct��Cv��Cx��Cz��C|��C~��C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�D !�D ��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D	!�D	��D
!�D
��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D !�D ��D!!�D!��D"!�D"��D#!�D#��D$!�D$��D%!�D%��D&!�D&��D'!�D'��D(!�D(��D)!�D)��D*!�D*��D+!�D+��D,!�D,��D-!�D-��D.!�D.��D/!�D/��D0!�D0��D1!�D1��D2!�D2��D3!�D3��D4!�D4��D5!�D5��D6!�D6��D7!�D7��D8!�D8��D9!�D9��D:!�D:��D;!�D;��D<!�D<��D=!�D=��D>!�D>��D?!�D?��D@!�D@��DA!�DA��DB!�DB��DC!�DC��DD!�DD��DE!�DE��DF!�DF��DG!�DG��DH!�DH��DI!�DI�RDJ!�DJ��DK!�DK��DL!�DL��DM!�DM��DN!�DN��DO!�DO��DP!�DP��DQ!�DQ��DR!�DR��DS!�DS��DT!�DT��DU!�DU��DV!�DV��DW!�DW��DX!�DX��DY!�DY��DZ!�DZ��D[!�D[��D\!�D\��D]!�D]��D^!�D^��D_!�D_��D`!�D`��Da!�Da��Db!�Db��Dc!�Dc��Dd!�Dd��De!�De��Df!�Df��Dg!�Dg��Dh!�Dh��Di!�Di��Dj!�Dj��Dk!�Dk��Dl!�Dl��Dm!�Dm��Dn!�Dn��Do!�Do��Dp!�Dp��Dq!�Dq��Dr!�Dr��Ds!�Ds��Dt!�Dt��Dt�Dy�D�-�D�W\D���D���D��D�]�D���D���D��D�`�D���D���D��D�P�Dڤ)D���D��D�D)D�}�D�ʐ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A߁A߁A߅A߇+A߅A߇+A߅AߋDAߋDA߇+A߇+A�~�A�z�A�|�A�z�A�v�A�t�A�x�A�v�A�t�A�v�A�v�A�l�A�dZA�^5A�ZA�VA�S�A�G�A�/Aޡ�AܓuA�~�A��`A�t�A�ĜA�C�Aմ9A�%A�;dAӝ�AҲ-A�"�A��TAϋDA�VA̶FA�E�A�G�A�VA�S�A���A���A�z�A���A�(�Aħ�A�v�A�|�A�E�A�A�A�-A���A��9A���A���A�|�A��A��/A�v�A���A�E�A���A��9A���A�{A�E�A��-A�
=A�ȴA��9A���A�C�A�ƨA��A�v�A�XA�A�bNA�^5A�E�A�9XA�7LA��DA���A��
A���A�-A�  A��A��A�z�A{�Aw%Aqp�Am��Ai&�Af�9Ae\)Aa�mA]�TA[VAW�AV-AUG�AT��ASXAP��ANE�AL��AK�^AK33AI��AH  AG%AD�AC�PAC&�ABȴAB(�A@�9A<-A:bA9;dA8I�A6=qA4��A4�\A4ZA2��A/`BA.�RA.v�A.bA*��A)��A)+A(�jA(^5A'XA&�RA%��A%p�A%
=A#t�A"Q�A"ZA!�A �A��A;dA(�A��AXA�9A�A�-A/A�DA�A��A�wAr�A�A�A�\A�hA�DA��A�AAA�9A=qA&�A
�+A
n�A
�\A
�uA
��AoA
n�A	�A	��A	��A��A��A�RA~�A	l�A	+Az�A1'A�HA��Ar�A�A�A=qA(�A�RA��Ax�A��AA�A�mA�wA�-A��A|�AA ��A �DA Z@�dZ@�+@��R@�p�@�Z@�o@�@��D@��@��@��@�Q�@�;d@�
=@�ff@��`@�ƨ@�+@��#@�@��-@�@�hs@�?}@��`@�@�D@�9X@�  @��;@��@�n�@��T@�G�@� �@��@��m@�w@�S�@��@�v�@�7L@��@�l�@�@�^5@�5?@���@��/@�(�@�C�@��@�7L@ܣ�@� �@�|�@�@ڟ�@�=q@٩�@��@ش9@�A�@���@���@�dZ@�S�@�33@��y@���@֟�@�J@���@�=q@Ցh@�?}@�7L@��@�9X@�(�@�9X@�  @ӥ�@�\)@�"�@���@�=q@�O�@�r�@�(�@�  @��;@ϥ�@�S�@��@��H@ΰ!@�J@��@��#@Ͳ-@�hs@�/@���@�Q�@��m@˶F@�dZ@�
=@ʧ�@ɡ�@�O�@��@��@��@�1@�\)@�K�@�"�@���@�^5@Ų-@�7L@���@Ĵ9@Ĵ9@Ĵ9@ċD@�Z@�1'@ÍP@�;d@��@���@���@�X@��@���@�I�@���@�|�@�+@���@�ff@�V@��@��T@��-@�O�@��@��@��@�V@��/@�z�@��@�o@��@��T@���@�@�@���@�x�@�hs@�`B@�G�@�1@�|�@�33@��H@�M�@��^@�X@��@�l�@�C�@�o@��H@���@�v�@�^5@�$�@���@�V@�r�@�j@�I�@���@��F@���@�dZ@��y@���@�5?@��@�$�@��@��@���@���@�7L@��@��@� �@�1@��m@�ƨ@��@���@��@�ȴ@�~�@�-@��^@�x�@���@��@���@���@�r�@�Q�@�ƨ@��@�n�@���@���@�hs@�?}@�%@��u@�Z@���@�K�@��@��y@�ȴ@���@�E�@��T@���@��7@�hs@�/@���@��/@�I�@���@��w@��F@���@���@�dZ@�+@���@��!@�v�@�E�@���@��7@�V@��j@��D@�Q�@��w@�hs@��@��@�t�@
=@u`B@j^5@_�;@X�@N��@D�@<z�@3��@.ff@&$�@!x�@1@b@C�@K�@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A߁A߁A߅A߇+A߅A߇+A߅AߋDAߋDA߇+A߇+A�~�A�z�A�|�A�z�A�v�A�t�A�x�A�v�A�t�A�v�A�v�A�l�A�dZA�^5A�ZA�VA�S�A�G�A�/Aޡ�AܓuA�~�A��`A�t�A�ĜA�C�Aմ9A�%A�;dAӝ�AҲ-A�"�A��TAϋDA�VA̶FA�E�A�G�A�VA�S�A���A���A�z�A���A�(�Aħ�A�v�A�|�A�E�A�A�A�-A���A��9A���A���A�|�A��A��/A�v�A���A�E�A���A��9A���A�{A�E�A��-A�
=A�ȴA��9A���A�C�A�ƨA��A�v�A�XA�A�bNA�^5A�E�A�9XA�7LA��DA���A��
A���A�-A�  A��A��A�z�A{�Aw%Aqp�Am��Ai&�Af�9Ae\)Aa�mA]�TA[VAW�AV-AUG�AT��ASXAP��ANE�AL��AK�^AK33AI��AH  AG%AD�AC�PAC&�ABȴAB(�A@�9A<-A:bA9;dA8I�A6=qA4��A4�\A4ZA2��A/`BA.�RA.v�A.bA*��A)��A)+A(�jA(^5A'XA&�RA%��A%p�A%
=A#t�A"Q�A"ZA!�A �A��A;dA(�A��AXA�9A�A�-A/A�DA�A��A�wAr�A�A�A�\A�hA�DA��A�AAA�9A=qA&�A
�+A
n�A
�\A
�uA
��AoA
n�A	�A	��A	��A��A��A�RA~�A	l�A	+Az�A1'A�HA��Ar�A�A�A=qA(�A�RA��Ax�A��AA�A�mA�wA�-A��A|�AA ��A �DA Z@�dZ@�+@��R@�p�@�Z@�o@�@��D@��@��@��@�Q�@�;d@�
=@�ff@��`@�ƨ@�+@��#@�@��-@�@�hs@�?}@��`@�@�D@�9X@�  @��;@��@�n�@��T@�G�@� �@��@��m@�w@�S�@��@�v�@�7L@��@�l�@�@�^5@�5?@���@��/@�(�@�C�@��@�7L@ܣ�@� �@�|�@�@ڟ�@�=q@٩�@��@ش9@�A�@���@���@�dZ@�S�@�33@��y@���@֟�@�J@���@�=q@Ցh@�?}@�7L@��@�9X@�(�@�9X@�  @ӥ�@�\)@�"�@���@�=q@�O�@�r�@�(�@�  @��;@ϥ�@�S�@��@��H@ΰ!@�J@��@��#@Ͳ-@�hs@�/@���@�Q�@��m@˶F@�dZ@�
=@ʧ�@ɡ�@�O�@��@��@��@�1@�\)@�K�@�"�@���@�^5@Ų-@�7L@���@Ĵ9@Ĵ9@Ĵ9@ċD@�Z@�1'@ÍP@�;d@��@���@���@�X@��@���@�I�@���@�|�@�+@���@�ff@�V@��@��T@��-@�O�@��@��@��@�V@��/@�z�@��@�o@��@��T@���@�@�@���@�x�@�hs@�`B@�G�@�1@�|�@�33@��H@�M�@��^@�X@��@�l�@�C�@�o@��H@���@�v�@�^5@�$�@���@�V@�r�@�j@�I�@���@��F@���@�dZ@��y@���@�5?@��@�$�@��@��@���@���@�7L@��@��@� �@�1@��m@�ƨ@��@���@��@�ȴ@�~�@�-@��^@�x�@���@��@���@���@�r�@�Q�@�ƨ@��@�n�@���@���@�hs@�?}@�%@��u@�Z@���@�K�@��@��y@�ȴ@���@�E�@��T@���@��7@�hs@�/@���@��/@�I�@���@��w@��F@���@���@�dZ@�+@���@��!@�v�@�E�@���@��7@�V@��j@��D@�Q�G�O�@�hs@��@��@�t�@
=@u`B@j^5@_�;@X�@N��@D�@<z�@3��@.ff@&$�@!x�@1@b@C�@K�@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
!�B
 �B
!�B
"�B
"�B
"�B
&�B
+B
/B
2-B
49B
6FB
:^B
=qB
G�B
gmB
jB
z�B
�XB
�`B
��B
��B%BbB�B'�B<jBF�BF�BT�Bm�Br�B�1B��B�B�jB��B��B+B%B��BPB$�B+B33B>wB\)BiyBt�Bz�B}�B~�Bq�BZBR�BM�B>wB+B!�BDB�sB�)B��B�B�)B�ZB�ZB�)BŢB��B�qB�?B��B�BjBK�B �BB
�ZB
ǮB
��B
{�B
gmB
N�B
&�B	��B	�NB	ĜB	��B	� B	bNB	S�B	I�B	6FB	#�B	�B	DB	B	B��B��B�B�B�sB�fB�ZB�NB�BB�/B�#B�B�B�B��B��B��B��B��B�#B�ZB�B�B�B�B�B�B�B�B��B	bB	�B	%�B	%�B	&�B	1'B	>wB	A�B	F�B	N�B	T�B	k�B	{�B	�%B	�VB	�PB	�DB	�VB	�oB	�uB	�bB	�\B	�DB	�1B	�+B	�%B	�B	�B	��B	��B	�7B	�+B	��B	�B	��B	��B	�PB	�B	� B	|�B	y�B	y�B	|�B	}�B	�7B	�PB	�VB	�VB	��B	��B	��B	��B	��B	��B	�?B	�9B	�3B	�}B	�XB	�LB	�FB	�'B	�FB	�^B	�^B	�FB	�'B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	��B	��B	�B	�B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�!B	�!B	�'B	�-B	�3B	�3B	�9B	�?B	�9B	�FB	�LB	�LB	�XB	�XB	�XB	�^B	�dB	�qB	�qB	�}B	��B	��B	ÖB	ƨB	ƨB	ƨB	ȴB	��B	��B	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�#B	�)B	�5B	�;B	�BB	�BB	�HB	�NB	�NB	�NB	�TB	�TB	�TB	�NB	�HB	�TB	�TB	�TB	�TB	�TB	�TB	�TB	�TB	�TB	�ZB	�ZB	�ZB	�ZB	�ZB	�ZB	�ZB	�`B	�fB	�fB	�mB	�mB	�sB	�sB	�sB	�sB	�sB	�sB	�sB	�yB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
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
%B
%B
%B
%B
%B
+B
+B
%B
%B
%B
%B
+B
+B
+B
+B
+B
+B
+B
1B
	7B
	7B
	7B
	7B
	7B
	7B
	7B

=B

=B

=B

=B

=B

=B

=B

=B
DB
DB

=B

=B
DB
DB

=B
DB
DB
DB
DB
DB
DB
JB
JB
JB
�B
VB
oB
�B
�B
(�B
5?B
<jB
C�B
G�B
K�B
N�B
S�B
]/B
`BB
e`B
iyB
n�B
s�B
x�B
|�B
� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B
�B
�B
�B
�B
�B
�B
�B
}B
{B
�B
�B
�B
�B
�B
�B
 �B
!�B
 �B
!�B
"�B
"�B
"�B
&�B
*�B
.�B
2B
4B
6B
:5B
=GB
G�B
gCB
jUB
z�B
�,B
�6B
��B
��B�B4BiB'�B<?BFBF}BT�BmhBr�B�B��B��B�<B��B��B�B�B��B!B$�B*�B3B>GB[�BiIBt�Bz�B}�B~�Bq{BY�BR�BM�B>HB*�B!�BB�DB��B��B��B��B�)B�)B��B�sB�QB�>B�B��B��BjOBK�B �B�B
�'B
�}B
��B
{�B
g8B
N�B
&�B	��B	�B	�eB	�hB	�B	bB	S�B	I�B	6B	#�B	VB	
B	�B	�B��B��B�jB�MB�7B�/B� B�B�
B��B��B��B��B��B��BϦBЪBϢBΞB��B�B�@B�^B�gB�cB�oB�nB�nB�dB��B	$B	IB	%�B	%�B	&�B	0�B	>:B	AMB	FkB	N�B	T�B	kIB	{�B	��B	�B	�B	�B	�B	�/B	�4B	�%B	�B	�B	��B	��B	��B	��B	��B	�DB	�bB	��B	��B	��B	��B	��B	�RB	�B	��B	�B	|�B	y�B	y�B	|�B	}�B	��B	�B	�B	�B	�BB	�xB	�`B	�eB	��B	��B	��B	��B	��B	�>B	�B	�B	�B	��B	�B	�B	� B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�B	�B	�#B	�/B	�0B	�:B	�@B	�EB	�TB	�hB	�dB	�eB	�rB	�|B	˅B	�vB	�vB	�yB	�}B	˄B	˃B	̌B	̊B	̊B	͎B	͐B	͑B	ΗB	ϜB	ѪB	ҮB	ӶB	ԻB	ԽB	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�B	�B	�B	�B	�B	�B	�	B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�!B	�+B	�*B	�-B	�1B	�-B	�0B	�/B	�1B	�/B	�4B	�3B	�7B	�9B	�7B	�BB	�IB	�HB	�EB	�GB	�IB	�DB	�DB	�IB	�PB	�TB	�NB	�LB	�SB	�YB	�]B	�[B	�bB	�eB	�^B	�_B	�gB	�hB	�gB	�iB	�eB	�mB	�pB	�sB	�rB	�qB	�qB	�pB	�pB	�zB	�zB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	�B
	�B
	�B
	�B
	�B
	�B
	�B
	�B
B

�B
	�B
	�B

�B
 B
	�B

�B

�B

�B
 B
B

�B
B
B
G�O�B
B
*B
[B
{B
(�B
4�B
<%B
CQB
GkB
K�B
N�B
S�B
\�B
_�B
eB
i2B
nVB
srB
x�B
|�B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.53 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             201904051007562019040510075620190405100756  AO  ARCAADJP                                                                    20181121125948    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181121125948  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181121125948  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190405100756  IP                  G�O�G�O�G�O�                