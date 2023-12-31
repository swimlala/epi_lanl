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
_FillValue                    �,Argo profile    3.1 1.2 19500101000000  20181121125947  20190405100755  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  5287                            2C  D   APEX                            6529                            072314                          846 @�̙j ν1   @�̚�J�@0�������d�G�z�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   A   A   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
�C�C�fC  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dtl�Dy�3D�	�D�0 D��fD��fD�  D�C3D��3D�ٚD��D�VfD���D��3D�  D�VfD�i�D��fD��D�&fD�l�D�ff11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��R@�Q�A(�A((�AH(�Ah(�A�{A�{A�{A�{A�{A�{A�{A�{B
=B

=B
=B
=B"
=B*
=B2
=B:
=BB
=BJ
=BR
=BZ
=Bb
=Bj
=Br
=Bz
=B�B�B�8RB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�C ��C��C��C��C��C
�)C�)Ch�C��C��C��C��C��C��C��C��C ��C"��C$��C&��C(��C*��C,��C.��C0��C2��C4��C6��C8��C:��C<��C>��C@��CB��CD��CF��CH��CJ��CL��CN��CP��CR��CT��CV��CX��CZ��C\��C^��C`��Cb��Cd��Cf��Ch��Cj��Cl��Cn��Cp��Cr��Ct��Cv��Cx��Cz��C|��C~��C�AHC�AHC�AHC�AHC�AHC�AHC�AHC�NC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHD  �D ��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D	 �D	��D
 �D
��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D�
D �D��D �D��D �D��D �D��D �D��D  �D ��D! �D!��D" �D"��D# �D#��D$ �D$��D% �D%��D& �D&��D' �D'��D( �D(��D) �D)��D* �D*��D+ �D+��D, �D,��D- �D-��D. �D.��D/ �D/��D0 �D0��D1 �D1��D2 �D2��D3 �D3��D4 �D4��D5 �D5��D6 �D6��D7 �D7��D8 �D8��D9 �D9��D: �D:��D; �D;��D< �D<��D= �D=��D> �D>��D? �D?��D@ �D@��DA �DA��DB �DB��DC �DC��DD �DD��DE �DE��DF �DF��DG �DG��DH �DH��DI �DI��DJ �DJ��DK �DK��DL �DL��DM �DM��DN �DN��DO �DO��DP �DP��DQ �DQ��DR �DR��DS �DS��DT �DT��DU �DU��DV �DV��DW �DW��DX �DX��DY �DY��DZ �DZ��D[ �D[��D\ �D\��D] �D]��D^ �D^��D_ �D_��D` �D`��Da �Da��Db �Db��Dc �Dc��Dd �Dd��De �De��Df �Df��Dg �Dg��Dh �Dh��Di �Di��Dj �Dj��Dk �Dk��Dl �Dl��Dm �Dm��Dn �Dn��Do �Do��Dp �Dp��Dq �Dq��Dr �Dr��Ds �Ds��Dt �Dt�qDy��D��D�@RD���D��D�RD�S�D���D���D�D�f�D��D��D�RD�f�D�y�D�ָD���D�6�D�}D�v�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�hA�PA�DA�7A�+A�A�~�A�x�A�r�A�jA�^5A�S�A�I�A�C�A�7LA�(�A�{A�
=A�%A�  A���A��mA���A��A�r�A��A��A��A�A޸RA�Q�A��#A���Aٛ�A�ZA���A�|�A�5?A��A��TA�(�AУ�A�;dA���A�p�A���A�ĜA�hsAˉ7A��AʃAɝ�A�S�A�XA�~�AŋDA�5?A�jA�A���A�/A�t�A��/A�Q�A�1A�A�hsA�1A�bA��uA���A��A�;dA�ZA�hsA���A��7A� �A�t�A�ƨA�t�A�A��A���A���A�dZA�A��A�x�A���A�ZA�^5A��DA�l�A�~�A��7A��A���A�C�A�r�A�=qA���A�%A�A�\)A�~�A�$�A�r�A~��AzAr��Ap�Ao�Am�FAk�Ahz�Ae��Acl�A]��AY�AX^5AV  AT{ARQ�AP��AN�jAL�HAJ�!AIS�AH �AF��AE��AD�AB��A@ȴA?K�A>JA<�A;\)A:v�A9��A8��A7�A7x�A7;dA6  A5�wA5;dA3�A2�A2VA0��A/��A,��A+33A)�A(�A($�A'ƨA'`BA&��A&$�A%�A%��A%l�A#x�A"JA!l�A!7LA �/A n�At�AJAv�A-A�;A�^A��A�!A �AJAZAM�A�A�wAVAr�AI�A�A�FA�A�A%AZA��Al�AoA��A  Al�AVA
��A
~�A
�\A
��A
ffA	�wA	��A	�^A	�
A	�A��A�wA+A�RA��Al�AC�A�!A��A�AhsA�AJA��At�A ��A b@�$�@��@��@�bN@��;@��@�"�@���@��^@�p�@��@�bN@���@�t�@�@�n�@��T@�`B@��/@�@��H@��T@�1'@�ȴ@홚@�w@�"�@�V@�G�@蛦@�@�9X@��@��@��@�C�@�+@���@⟾@�=q@��T@ᙚ@�`B@�9X@��@߮@�t�@�\)@�+@�"�@�o@��@ޏ\@�^5@�M�@��@�?}@�A�@ە�@ڗ�@�=q@١�@��`@�j@�1@��m@ו�@�$�@��@Լj@ԓu@�z�@�I�@��@Ӆ@��y@�&�@д9@У�@�r�@ϕ�@�~�@͉7@�&�@��@�S�@�33@��@ʧ�@�ff@�{@ə�@�X@��@���@��/@ȼj@Ȭ@�bN@��@���@�"�@Ɨ�@�E�@��@ŉ7@�O�@�&�@�%@��`@���@Ĭ@ě�@ċD@�z�@�Z@�(�@�1@��;@Õ�@�dZ@+@�?}@�/@��`@�z�@�9X@���@�t�@��H@��!@��\@��@���@�X@���@���@�j@� �@��m@�o@��!@�$�@�@��@�`B@�j@���@��@��@�"�@��H@���@�ff@�5?@��^@�O�@��`@�Z@�A�@���@��@��@��y@�^5@�{@�@��T@���@��@��@�p�@�/@���@��9@�j@��m@��@�+@��y@���@�$�@��7@���@�z�@�A�@��@��F@�|�@�@�@��@��@��y@��@��#@��@�&�@��@��@�7L@��`@�1'@��m@��F@�|�@�;d@��y@�~�@�M�@�-@�{@��#@�&�@���@���@�z�@�bN@�I�@�(�@��;@��
@��@��H@�~�@�^5@��@�p�@�V@���@��j@�Q�@�(�@��F@�\)@�"�@�n�@�@��^@�G�@�%@���@�bN@�b@���@�|�@�;d@��@��R@��!@���@��!@���@���@�M�@�M�@���@���@�7L@���@�V@���@���@�~�@{@u�-@lj@b�@Z��@Q�@E�h@<�D@4��@.��@&ȴ@!�@ƨ@�@z�@�#@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�hA�PA�DA�7A�+A�A�~�A�x�A�r�A�jA�^5A�S�A�I�A�C�A�7LA�(�A�{A�
=A�%A�  A���A��mA���A��A�r�A��A��A��A�A޸RA�Q�A��#A���Aٛ�A�ZA���A�|�A�5?A��A��TA�(�AУ�A�;dA���A�p�A���A�ĜA�hsAˉ7A��AʃAɝ�A�S�A�XA�~�AŋDA�5?A�jA�A���A�/A�t�A��/A�Q�A�1A�A�hsA�1A�bA��uA���A��A�;dA�ZA�hsA���A��7A� �A�t�A�ƨA�t�A�A��A���A���A�dZA�A��A�x�A���A�ZA�^5A��DA�l�A�~�A��7A��A���A�C�A�r�A�=qA���A�%A�A�\)A�~�A�$�A�r�A~��AzAr��Ap�Ao�Am�FAk�Ahz�Ae��Acl�A]��AY�AX^5AV  AT{ARQ�AP��AN�jAL�HAJ�!AIS�AH �AF��AE��AD�AB��A@ȴA?K�A>JA<�A;\)A:v�A9��A8��A7�A7x�A7;dA6  A5�wA5;dA3�A2�A2VA0��A/��A,��A+33A)�A(�A($�A'ƨA'`BA&��A&$�A%�A%��A%l�A#x�A"JA!l�A!7LA �/A n�At�AJAv�A-A�;A�^A��A�!A �AJAZAM�A�A�wAVAr�AI�A�A�FA�A�A%AZA��Al�AoA��A  Al�AVA
��A
~�A
�\A
��A
ffA	�wA	��A	�^A	�
A	�A��A�wA+A�RA��Al�AC�A�!A��A�AhsA�AJA��At�A ��A b@�$�@��@��@�bN@��;@��@�"�@���@��^@�p�@��@�bN@���@�t�@�@�n�@��T@�`B@��/@�@��H@��T@�1'@�ȴ@홚@�w@�"�@�V@�G�@蛦@�@�9X@��@��@��@�C�@�+@���@⟾@�=q@��T@ᙚ@�`B@�9X@��@߮@�t�@�\)@�+@�"�@�o@��@ޏ\@�^5@�M�@��@�?}@�A�@ە�@ڗ�@�=q@١�@��`@�j@�1@��m@ו�@�$�@��@Լj@ԓu@�z�@�I�@��@Ӆ@��y@�&�@д9@У�@�r�@ϕ�@�~�@͉7@�&�@��@�S�@�33@��@ʧ�@�ff@�{@ə�@�X@��@���@��/@ȼj@Ȭ@�bN@��@���@�"�@Ɨ�@�E�@��@ŉ7@�O�@�&�@�%@��`@���@Ĭ@ě�@ċD@�z�@�Z@�(�@�1@��;@Õ�@�dZ@+@�?}@�/@��`@�z�@�9X@���@�t�@��H@��!@��\@��@���@�X@���@���@�j@� �@��m@�o@��!@�$�@�@��@�`B@�j@���@��@��@�"�@��H@���@�ff@�5?@��^@�O�@��`@�Z@�A�@���@��@��@��y@�^5@�{@�@��T@���@��@��@�p�@�/@���@��9@�j@��m@��@�+@��y@���@�$�@��7@���@�z�@�A�@��@��F@�|�@�@�@��@��@��y@��@��#@��@�&�@��@��@�7L@��`@�1'@��m@��F@�|�@�;d@��y@�~�@�M�@�-@�{@��#@�&�@���@���@�z�@�bN@�I�@�(�@��;@��
@��@��H@�~�@�^5@��@�p�@�V@���@��j@�Q�@�(�@��F@�\)@�"�@�n�@�@��^@�G�@�%@���@�bN@�b@���@�|�@�;d@��@��R@��!@���@��!@���@���@�M�@�M�@���@���@�7L@���@�V@���@���@�~�@{@u�-@lj@b�@Z��@Q�@E�h@<�D@4��@.��@&ȴ@!�@ƨ@�@z�@�#@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB:^B8RB49B1'B-B)�B%�B!�B�B�B{BbB
=BB��B��B�B�`B��B�}B��B�1BffBB�B�B
��B
�B
��B
�RB
��B
�B'�B-B�B
��B  B+B�B?}BcTBt�Bw�Bn�B]/B+B
�B
�B�B%�B�B
�B
�/B
��BVB�B)�BcTBhsB_;BjB�B��B�LB��B��B��B�#B�NB��BiyB�+By�Be`BN�BE�B?}B-B�B�BuBbBDBB�B�yB�BǮB�-B�B^5B@�B+B1B
�B
�HB
��B
��B
�LB
��B
�+B
q�B
dZB
K�B
0!B
�B	��B	�NB	��B	�B	�\B	� B	u�B	k�B	cTB	dZB	[#B	N�B	E�B	1'B	 �B	�B	bB	DB	B��B��B�B�B�yB�fB�TB�HB�BB�5B�)B�B�B�B�#B�;B�;B�NB�sB�B�B�B��B��B	1B	\B	�B	!�B	&�B	'�B	(�B	-B	)�B	(�B	(�B	)�B	.B	A�B	J�B	O�B	P�B	R�B	S�B	R�B	Q�B	P�B	N�B	J�B	O�B	T�B	S�B	S�B	R�B	P�B	O�B	L�B	D�B	A�B	B�B	C�B	B�B	E�B	H�B	L�B	M�B	L�B	F�B	?}B	?}B	>wB	B�B	G�B	J�B	I�B	J�B	K�B	L�B	O�B	W
B	`BB	iyB	u�B	|�B	�=B	�VB	��B	��B	�B	�3B	�9B	�9B	�9B	�'B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�'B	�-B	�3B	�FB	�FB	�RB	�RB	�^B	�}B	��B	B	ÖB	ĜB	ĜB	ĜB	ÖB	B	B	��B	�}B	�qB	�qB	�qB	�wB	�wB	�wB	�wB	�}B	�}B	�}B	��B	��B	ÖB	ĜB	ƨB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�B	�B	�/B	�5B	�;B	�;B	�;B	�;B	�5B	�5B	�/B	�/B	�/B	�/B	�;B	�BB	�HB	�HB	�HB	�HB	�HB	�HB	�HB	�HB	�BB	�HB	�HB	�BB	�BB	�HB	�NB	�`B	�`B	�fB	�mB	�mB	�mB	�mB	�mB	�mB	�sB	�yB	�yB	�yB	�yB	�yB	�yB	�yB	�yB	�yB	�yB	�yB	�yB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
  B
  B
B
B
B
  B
B
B
B
B
B
B
B
B
B
B
B
%B
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
%B
+B
1B
1B
1B
1B
1B
	7B
	7B

=B

=B

=B

=B

=B
	7B
	7B
	7B
	7B
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
DB
DB
DB
JB
JB
JB
JB
JB
JB
PB
\B
\B
\B
\B
\B
\B
\B
hB
\B
�B
�B
#�B
,B
9XB
:^B
@�B
E�B
K�B
S�B
XB
]/B
`BB
e`B
k�B
q�B
t�B
w�B
y�B
|�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B:8B8+B4B0�B,�B)�B%�B!�B�BoBUB;B
B�B��B��B�|B�7B��B�VB��B�
Bf@BBhB�B
��B
��B
�bB
�+B
ϷB
�B'�B,�BwB
��B
��BB�B?TBc*Bt�Bw�BnpB]B*�B
�[B
�B�B%�BqB
�ZB
�B
��B,BwB)�Bc(BhJB_BjQB��B��B�"B��B��B��B��B�"B��BiKB��By�Be2BN�BEvB?QB,�ByBeBKB2BB�B�B�IB��B�~B��B��B^B@SB*�BB
�gB
�B
��B
ʑB
�B
��B
��B
qyB
d(B
K�B
/�B
{B	��B	�B	�XB	��B	�(B	�B	u�B	kQB	c B	d$B	Z�B	N�B	EmB	0�B	 �B	kB	+B	B	�B��B��B�B�eB�BB�/B�B�B�	B��B��B��B��B��B��B� B�B�B�9B�RB�YB�|B��B��B	�B	"B	GB	!�B	&�B	'�B	(�B	,�B	)�B	(�B	(�B	)�B	-�B	AOB	J�B	O�B	P�B	R�B	S�B	R�B	Q�B	P�B	N�B	J�B	O�B	T�B	S�B	S�B	R�B	P�B	O�B	L�B	D`B	ANB	BTB	CWB	BSB	EhB	HxB	L�B	M�B	L�B	FiB	?@B	?=B	>9B	BPB	GrB	J�B	I~B	J�B	K�B	L�B	O�B	V�B	`B	i<B	u�B	|�B	�B	�B	�|B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�yB	�pB	�xB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	� B	�>B	�CB	�QB	�WB	�\B	�^B	�[B	�UB	�PB	�QB	�HB	�?B	�0B	�1B	�1B	�8B	�8B	�:B	�9B	�>B	�;B	�=B	�BB	�HB	�WB	�\B	�gB	�lB	�vB	ʁB	̌B	ˈB	ΘB	ѬB	ӺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	�B	�B	�B	�	B	�	B	�B	�B	�B	�B	�B	�B	�B	� B	�B	�B	� B	�#B	�&B	�+B	�,B	�-B	�,B	�/B	�.B	�3B	�:B	�6B	�7B	�8B	�7B	�7B	�9B	�5B	�9B	�7B	�:B	�8B	�7B	�:B	�8B	�9B	�>B	�;B	�?B	�AB	�=B	�=B	�DB	�DB	�DB	�CB	�DB	�EB	�BB	�HB	�PB	�PB	�XB	�TB	�XB	�]B	�]B	�eB	�dB	�cB	�bB	�lB	�kB	�hB	�oB	�oB	�hB	�lB	�jB	�sB	�uB	�oB	�hB	�cB	�YB	�^B	�`B	�aB	�bB	�nB	�{B	�|B	�yB	��B	�|B	�zB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B	��B	��B
 �B
 �B
 �B	��B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
	�B
	�B
	�B
	�B
	�B
�B
�B
�B
�B
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
B
B
 B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
'B
B
qB
vB
#�B
+�B
9B
:B
@?B
E_B
K�B
S�B
W�B
\�B
` B
eB
kEB
qhB
tyB
w�B
y�B
|�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.51 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             201904051007552019040510075520190405100755  AO  ARCAADJP                                                                    20181121125947    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181121125947  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181121125947  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190405100755  IP                  G�O�G�O�G�O�                