CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-11-21T12:59:47Z creation      
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
_FillValue                    �,Argo profile    3.1 1.2 19500101000000  20181121125947  20190405100755  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  5287                            2C  D   APEX                            6529                            072314                          846 @��<�=-�1   @��=-���@0������d{�O�;d1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   A   A   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C�C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4�C6�C8�C:  C;�fC=�fC@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^�C`�Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<y�D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dts3Dy�fD�3D�FfD�� D�ɚD�#3D�L�D�l�D�� D�3D�33D�� D�� D�3D�C3D�y�D�ɚD�3D�9�D�fD�\�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��]@���Az�A(z�AHz�Ahz�A�=qA�=qA�=qA�=qA�=qA�=qA�=qA�=qB�B
�B�B�B"�B*�B2�B:�BB�BJ�BR�BZ�Bb�Bj�Br�Bz�B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�B�B�B�B�\B�\B�\B�\B�\B�\B�\B�\B�\B�B�B�\B�\B�\B�\B�\B�\B�\B�\C ��C��C��C��C��C
��C��C��C�HC��C��C��C��C��C��C��C ��C"��C$��C&��C(��C*��C,��C.��C0��C2��C4�HC6�HC8�HC:��C<nC>nC@��CB��CD��CF��CH��CJ��CL��CN��CP��CR��CT��CV��CX��CZ��C\��C^�HC`�HCb��Cd��Cf��Ch��Cj��Cl��Cn��Cp��Cr��Ct��Cv��Cx��Cz��C|��C~��C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�P�C�P�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�D !�D ��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D	!�D	��D
!�D
��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D !�D ��D!!�D!��D"!�D"��D#!�D#��D$!�D$��D%!�D%��D&!�D&��D'!�D'��D(!�D(��D)!�D)��D*!�D*��D+!�D+��D,!�D,��D-!�D-��D.!�D.��D/!�D/��D0!�D0��D1!�D1��D2!�D2��D3!�D3��D4!�D4��D5!�D5��D6!�D6��D7!�D7��D8!�D8��D9!�D9��D:!�D:��D;!�D;��D<!�D<��D=!�D=��D>!�D>��D?!�D?��D@!�D@��DA!�DA��DB!�DB��DC!�DC��DD!�DD��DE!�DE��DF!�DF��DG!�DG��DH!�DH��DI!�DI��DJ!�DJ��DK!�DK��DL!�DL��DM!�DM��DN!�DN��DO!�DO��DP!�DP��DQ!�DQ��DR!�DR��DS!�DS��DT!�DT��DU!�DU��DV!�DV��DW!�DW��DX!�DX��DY!�DY��DZ!�DZ��D[!�D[��D\!�D\��D]!�D]��D^!�D^��D_!�D_��D`!�D`��Da!�Da��Db!�Db��Dc!�Dc��Dd!�Dd��De!�De��Df!�Df��Dg!�Dg��Dh!�Dh��Di!�Di��Dj!�Dj��Dk!�Dk��Dl!�Dl��Dm!�Dm��Dn!�Dn��Do!�Do��Dp!�Dp��Dq!�Dq��Dr!�Dr��Ds!�Ds��Dt!�Dt�Dy�RD�)D�W\D���D�ڐD�4)D�]�D�}�D���D�)D�D)D���D���D�)D�T)Dڊ�D�ڐD�)D�J�D�\D�m�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�K�A�I�A�E�A�C�A�-A� �A�{A�JA���A��A��HA���A���A�ĜA�A�A���A�wA�wA�A�^A�9A�A�A��A�\A�r�A�Q�A�5?A�A��A�n�A���A�v�Aܩ�A�bA��A�+AؑhA�jA�Q�A���Aҩ�A�bNA�A�A�{A�C�A�O�A���A�\)A��A�oAʙ�A�Q�A���A��AƁA�\)A�(�A�O�A��
A�t�A��mA��FA�ffA��;A�9XA�\)A�z�A���A���A��mA��A��A�bNA�9XA�l�A�
=A�I�A�n�A�l�A�JA��-A��;A�l�A�$�A�A��RA��A�/A�x�A�7LA���A�%A�$�A�v�A���A��yA��uA�`BA�+A�`BA�%A�r�A��A���A�K�A��A�v�A�oA���A���A~M�A}7LAzȴAs�Alr�Af1Ab��A^(�AX�jAT  AP��AN�HAM&�AK�AH�HAE%AB��AA��A?�#A<��A;��A:��A9�FA8��A7�
A6��A5��A5C�A3\)A1
=A0�uA/hsA-�^A+��A+O�A*�HA*JA)oA'��A&�/A&z�A&A�A%�;A%x�A%K�A%/A$�A$�jA$�uA$5?A#�hA#?}A#�A#
=A"�A"�yA"��A"�9A"�+A!�wA!�A �yA ��A �A �A ȴA �RA|�A�HA�RA��A�A��A�RA�DA��A�A��A��A�\A�FAx�AK�A�A��A�FA�Av�A�A�!Az�A1A�;A�7A�A1'A�AA�A��A�AA��A��AhsA/A��A��A��AbNA�A�-A��Al�A
��A
n�A	�;A	?}A�/A~�A(�A�^At�AdZA��A�Av�AJAx�A�AȴAA�A�mA��AbA �/@���@��@��/@��D@��@�+@�5?@���@�`B@���@��@�\)@�33@���@��^@��j@�w@�dZ@�~�@��@��@��y@�E�@�{@�x�@�`B@�`B@�@�I�@�1@�1@��;@�\)@�o@�@��@�^5@�h@�j@�@�l�@��@�C�@�dZ@��@��H@��@��@��@�1'@�=q@ᙚ@��@���@�Z@�ȴ@��/@�E�@�X@�/@���@ش9@�1'@�t�@�ȴ@�n�@�~�@ְ!@�=q@պ^@���@���@�
=@Ұ!@�5?@�G�@Л�@���@ϥ�@�33@ΰ!@�ff@�-@�O�@�Q�@˝�@�S�@�+@ʏ\@���@��@ȋD@�ƨ@Ǖ�@�S�@�33@��@Ə\@�ff@�{@��@��#@��#@Ų-@�G�@��@ļj@ě�@�Z@�ƨ@��@��@���@��h@�O�@��`@�Ĝ@��D@�I�@��m@�t�@�S�@�+@�@��@��H@���@�=q@�@��@�&�@��@��`@��u@�Q�@�A�@�1'@��w@�t�@�dZ@�K�@�+@�V@��@���@���@�z�@���@��w@�S�@���@��\@�{@�@���@�p�@�?}@��`@�j@�(�@�1@���@��@���@��@��H@�ff@��T@��7@�p�@�O�@���@���@��@���@�b@��;@�ƨ@���@�"�@���@�J@��h@�&�@���@�bN@�9X@��m@�t�@��y@�~�@�V@���@��h@���@�O�@���@���@��u@�  @��@�t�@�+@���@�^5@�^5@�-@��7@��@�Ĝ@�Z@���@��m@��w@��P@��@�V@�$�@�@�&�@��`@��9@��D@��@��P@�|�@�l�@�K�@�@���@��@���@���@���@�`B@���@��D@�(�@�  @�ƨ@�|�@��@�ȴ@�%@�b@�-@x �@pA�@g+@\�D@R�H@HQ�@Co@;��@65?@0r�@+33@$�/@5?@�@�h@�711111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�K�A�I�A�E�A�C�A�-A� �A�{A�JA���A��A��HA���A���A�ĜA�A�A���A�wA�wA�A�^A�9A�A�A��A�\A�r�A�Q�A�5?A�A��A�n�A���A�v�Aܩ�A�bA��A�+AؑhA�jA�Q�A���Aҩ�A�bNA�A�A�{A�C�A�O�A���A�\)A��A�oAʙ�A�Q�A���A��AƁA�\)A�(�A�O�A��
A�t�A��mA��FA�ffA��;A�9XA�\)A�z�A���A���A��mA��A��A�bNA�9XA�l�A�
=A�I�A�n�A�l�A�JA��-A��;A�l�A�$�A�A��RA��A�/A�x�A�7LA���A�%A�$�A�v�A���A��yA��uA�`BA�+A�`BA�%A�r�A��A���A�K�A��A�v�A�oA���A���A~M�A}7LAzȴAs�Alr�Af1Ab��A^(�AX�jAT  AP��AN�HAM&�AK�AH�HAE%AB��AA��A?�#A<��A;��A:��A9�FA8��A7�
A6��A5��A5C�A3\)A1
=A0�uA/hsA-�^A+��A+O�A*�HA*JA)oA'��A&�/A&z�A&A�A%�;A%x�A%K�A%/A$�A$�jA$�uA$5?A#�hA#?}A#�A#
=A"�A"�yA"��A"�9A"�+A!�wA!�A �yA ��A �A �A ȴA �RA|�A�HA�RA��A�A��A�RA�DA��A�A��A��A�\A�FAx�AK�A�A��A�FA�Av�A�A�!Az�A1A�;A�7A�A1'A�AA�A��A�AA��A��AhsA/A��A��A��AbNA�A�-A��Al�A
��A
n�A	�;A	?}A�/A~�A(�A�^At�AdZA��A�Av�AJAx�A�AȴAA�A�mA��AbA �/@���@��@��/@��D@��@�+@�5?@���@�`B@���@��@�\)@�33@���@��^@��j@�w@�dZ@�~�@��@��@��y@�E�@�{@�x�@�`B@�`B@�@�I�@�1@�1@��;@�\)@�o@�@��@�^5@�h@�j@�@�l�@��@�C�@�dZ@��@��H@��@��@��@�1'@�=q@ᙚ@��@���@�Z@�ȴ@��/@�E�@�X@�/@���@ش9@�1'@�t�@�ȴ@�n�@�~�@ְ!@�=q@պ^@���@���@�
=@Ұ!@�5?@�G�@Л�@���@ϥ�@�33@ΰ!@�ff@�-@�O�@�Q�@˝�@�S�@�+@ʏ\@���@��@ȋD@�ƨ@Ǖ�@�S�@�33@��@Ə\@�ff@�{@��@��#@��#@Ų-@�G�@��@ļj@ě�@�Z@�ƨ@��@��@���@��h@�O�@��`@�Ĝ@��D@�I�@��m@�t�@�S�@�+@�@��@��H@���@�=q@�@��@�&�@��@��`@��u@�Q�@�A�@�1'@��w@�t�@�dZ@�K�@�+@�V@��@���@���@�z�@���@��w@�S�@���@��\@�{@�@���@�p�@�?}@��`@�j@�(�@�1@���@��@���@��@��H@�ff@��T@��7@�p�@�O�@���@���@��@���@�b@��;@�ƨ@���@�"�@���@�J@��h@�&�@���@�bN@�9X@��m@�t�@��y@�~�@�V@���@��h@���@�O�@���@���@��u@�  @��@�t�@�+@���@�^5@�^5@�-@��7@��@�Ĝ@�Z@���@��m@��w@��P@��@�V@�$�@�@�&�@��`@��9@��D@��@��P@�|�@�l�@�K�@�@���@��@���@���@���@�`B@���@��D@�(�@�  @�ƨ@�|�@��@�ȴ@�%@�b@�-@x �@pA�@g+@\�D@R�H@HQ�@Co@;��@65?@0r�@+33@$�/@5?@�@�h@�711111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB

=B

=B

=B

=B
	7B

=B

=B

=B
JB
VB
oB
�B
�B
&�B
2-B
<jB
F�B
M�B
VB
\)B
^5B
`BB
bNB
cTB
cTB
ffB
k�B
o�B
v�B
��B
�-B
��B
ĜB
ǮB
B
��B
�JB
|�B
o�B
`BB
r�B
ZB
T�B
iyB
�uB
��B
�9B
�qB
ÖB
�B{B"�B'�BL�BiyB�%B�hB�B�^BȴB�
BB1BPBVBF�B^5BjBcTB[#B\)Bk�Bn�BgmBdZBcTBbNBffBcTBaHB]/B\)B[#BaHBe`BffBgmBbNBaHBXBO�B.B�BB�RB�+BS�B�BB
�B
�5B
��B
��B
�dB
�'B
��B
{�B
ZB
>wB
5?B
/B
&�B
\B	�B	�B	�B	�3B	�=B	ffB	O�B	8RB	�B	JB	  B��B�B�B�`B�#B��B��B�B�BB�BB�;B�5B�;B�BB�TB�B��B	B	
=B	DB	PB	DB��B��B	  B	B	B	JB	�B	.B	;dB	H�B	P�B	S�B	T�B	[#B	_;B	cTB	l�B	}�B	�B	�7B	�DB	�\B	�oB	��B	��B	��B	�B	�-B	�RB	�wB	ÖB	ȴB	��B	��B	��B	��B	��B	�B	�NB	�TB	�mB	�sB	�B	�B	�TB	�B	��B	��B	��B	��B	��B	��B	ǮB	ĜB	ÖB	�wB	�jB	�qB	�wB	ĜB	��B	ȴB	��B	ƨB	��B	�^B	�dB	ÖB	��B	��B	��B	��B	��B	��B	�
B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�B	�B	�B	�B	�B	�B	�)B	�#B	�B	�B	�
B	�B	�B	��B	ɺB	ŢB	B	ĜB	ĜB	ƨB	ǮB	��B	��B	��B	��B	��B	��B	��B	ɺB	ȴB	ǮB	ƨB	ɺB	ȴB	ȴB	ƨB	ƨB	ƨB	ƨB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�
B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	B	�}B	�}B	�}B	��B	��B	��B	ÖB	ĜB	ȴB	��B	��B	��B	�B	�B	�
B	�B	�B	�B	�#B	�)B	�)B	�/B	�/B	�/B	�/B	�;B	�HB	�NB	�NB	�NB	�NB	�NB	�NB	�TB	�ZB	�ZB	�ZB	�`B	�`B	�`B	�`B	�fB	�fB	�fB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
  B
  B
B
B
B
B
B
B
B
%B
%B
1B
1B
1B
1B
	7B
	7B
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
DB
DB

=B

=B

=B

=B

=B
DB
DB
DB
DB
DB
JB
JB
JB
PB
PB
PB
VB
\B
oB
�B
�B
$�B
-B
33B
;dB
@�B
C�B
K�B
N�B
T�B
ZB
_;B
cTB
iyB
l�B
p�B
s�B
w�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B

B

B

B

B
	B

B

B

B
"B
1B
GB
eB
�B
&�B
2B
<DB
F�B
M�B
U�B
\ B
^B
`B
b&B
c.B
c.B
f>B
k^B
ovB
v�B
��B
�B
�\B
�rB
ǃB
�cB
��B
�!B
|�B
otB
`B
r�B
Y�B
T�B
iKB
�MB
��B
�B
�GB
�jB
�|BNB"�B'�BL�BiMB��B�<B��B�0BȈB��B�BB#B+BFyB^	BjUBc%BZ�B[�BkVBnhBg?Bd-Bc%BbBf7Bc&BaB] B[�BZ�BaBe2Bf7Bg=Bb BaBW�BO�B-�B�B�B��BS�B�B �B
�cB
�B
̜B
�UB
�1B
��B
�fB
{�B
Y�B
>BB
5B
.�B
&�B
)B	�B	�NB	��B	��B	�B	f0B	O�B	8B	�B	B��B��B�B�XB�'B��B��BӾB��B�B�B��B��B��B�B�B�PB��B	�B	
B	B	B	B��B��B��B	�B	�B	B	vB	-�B	;)B	HvB	P�B	S�B	T�B	Z�B	^�B	cB	lLB	}�B	��B	��B	�B	� B	�3B	�\B	��B	��B	��B	��B	�B	�;B	�WB	�uB	̐B	͖B	ʁB	ʃB	ϢB	��B	�B	�B	�0B	�5B	�NB	�FB	�B	��B	ӼB	ϟB	ΛB	ΜB	͔B	ˇB	�pB	�]B	�YB	�:B	�+B	�1B	�9B	�]B	ʃB	�vB	ʂB	�hB	�CB	� B	�&B	�XB	�HB	͓B	ϠB	ЦB	ϡB	ԾB	��B	��B	ӻB	ӹB	ҲB	ҴB	ѫB	ѬB	ѬB	ӹB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ѪB	�xB	�aB	�MB	�ZB	�\B	�fB	�qB	̎B	͒B	ΘB	ΗB	̌B	ˈB	˄B	�{B	�rB	�mB	�fB	�xB	�rB	�rB	�gB	�dB	�hB	�dB	�lB	�~B	ϚB	ϝB	ϞB	УB	ұB	үB	ӸB	ԼB	ԽB	��B	ԽB	ӵB	үB	ѮB	ұB	үB	ԾB	��B	��B	��B	��B	ԽB	ԼB	ԻB	ӹB	ԺB	��B	��B	ԽB	ТB	ˇB	�NB	�:B	�9B	�:B	�?B	�?B	�>B	�SB	�YB	�qB	͏B	ϝB	ӳB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�
B	�B	�	B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�%B	�#B	�$B	�)B	�6B	�?B	�@B	�@B	�AB	�BB	�AB	�FB	�OB	�LB	�OB	�LB	�VB	�VB	�TB	�SB	�[B	�ZB	�[B	�XB	�YB	�YB	�ZB	�ZB	�aB	�`B	�eB	�gB	�mB	�lB	�sB	�wB	�wB	�xB	�yB	�xB	�{B	�yB	�rB	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
B
B
	�B
	�B
	�B
	�B
	�B

�B

�B

�B

�B
 B
B
B
B
B
B
B
B
B
+B
TB
yB
$�B
,�B
2�B
;!B
@<B
CSB
K�B
N�B
T�B
Y�B
^�B
cB
i4B
lDB
p`B
ssB
w�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.53 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             201904051007552019040510075520190405100755  AO  ARCAADJP                                                                    20181121125947    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181121125947  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181121125947  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190405100755  IP                  G�O�G�O�G�O�                