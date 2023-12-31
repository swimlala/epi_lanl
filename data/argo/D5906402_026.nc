CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2021-07-30T05:34:39Z creation; 2022-04-26T16:06:59Z DMQC;      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.10   Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_on_dac_decoder_version        $Decoded by SIO: Argo SIO SOLOII V2.6   comment_dmqc_operator         bPRIMARY | https://orcid.org/0000-0003-0805-6570 | John Gilson, Scripps Institution of Oceanography        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    8   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  �  8   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  �  8�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  `  9   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        9x   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    9�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    9�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                  @  9�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    9�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    9�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                  @  9�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                  @  :   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                  @  :T   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    :�   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   axis      T      
resolution        >�E�r�_K   
_FillValue        A.�~            :�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    :�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        ?F�k"kmj   
_FillValue        A.�~            :�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   	valid_min         �V�        	valid_max         @V�        axis      Y      
_FillValue        @�i�            :�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    	valid_min         �f�        	valid_max         @f�        axis      X      
_FillValue        @�i�            :�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    :�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    :�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    :�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    :�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    :�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    ;    CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        =    PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     @  =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 P  ZH   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     @  a�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 P  ~�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     @  �(   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     @  �h   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 P  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     @  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 P  �8   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     @  �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     @ 	�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 P '   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     @ .X   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 P K�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     @ R�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` p(   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   p�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   v�   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   |�   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   �|   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20210730053439  20220426232407  5906402 5906402 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH, SARAH PURKEY, NATHALIE ZILBERMAN, JOHN GILSON    DEAN ROEMMICH, SARAH PURKEY, NATHALIE ZILBERMAN, JOHN GILSON    PRES            TEMP            PSAL            PRES            TEMP            PSAL                  AA  AOAO8138_008904_026                 8138_008904_026                 2C  2C  DD  SOLO_II                         SOLO_II                         8904                            8904                            V2.6; SBE602 14Jan20            V2.6; SBE602 14Jan20            853 853 @ه����@ه����11  @ه�IQ��@ه�IQ��@,�+�C@,�+�C�d�p�Ͽ��d�p�Ͽ�11  GPS     GPS     BA  BA  BA  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?�=q@   @B�\@�G�@�  @��R@޸R@��RA��A ��A,(�AAG�A`��A�  A�  A�Q�A�  A��A�  A�Q�A�Q�B (�BQ�B(�B  B   B(  B0  B8  B?�
BG�
BO�
BX  B`Q�Bh(�Bp  Bw�
B�{B�{B�{B�{B�  B��B��B�  B�  B�  B�{B�{B�  B��B�  B��B�  B�  B�  B�  B�  B�  B�{B�{B�  B�  B�{B�{B��B��B�  B�  C   C
=C  C��C  C	��C��C
=C  C  C
=C
=C  C��C��C��C�C!��C$  C%��C(  C*
=C,
=C.
=C0  C1��C3��C5��C7��C:  C<
=C>
=C@  CB  CD  CF  CH
=CJ{CL  CN  CP  CQ��CS��CU��CX  CZ  C\  C^  C`  Cb  Cd  Cf
=Ch  Cj
=Cl  Cm�Co��Cq��Ct  Cv{Cx{Cz
=C{��C}��C�  C�C�C�  C�  C�  C�  C�  C�C�C�  C�  C�C�  C���C�  C�C�C�C�C�  C�  C�C�C�C�  C�  C�  C�  C�C�
=C�
=C�
=C�C�  C�  C�  C�  C�  C���C�  C�C�  C�  C�C�  C���C�  C�  C�  C�  C�C�C�C�C�C�  C�  C�  C�  C�  C�C�C�C�C�  C�C�C�  C���C�  C�C�  C�  C�  C�  C�C�C���C���C�  C�C���C�  C�C���C�  C�C�  C���C�  C�  C�  C�C�  C���C�C�  C���C�  C�C���C���C�  C�  C���C�C�C�C�C�C�
=C�
=C�
=C�  C���C�  C�
=C�C�C�  C���C���C�  C�
=C�
=C�C�D   D � D�D� D  D}qD�qD}qD  D� D  D��D�qD� D�D��DD��D	  D	}qD	�qD
� DD��D  Dz�D�qD}qD�qD� D�D� D�qD� D�D� D�D��D�D��D�D��D  Dz�D  D� D  D}qD  D� D  D��D  D��D  D� D  D� D  D��D  D}qD�qD��D   D }qD ��D!� D"  D"� D"�qD#}qD$�D$��D%�D%� D&  D&� D'�D'�D(�D(� D)  D)}qD)��D*}qD+  D+��D,  D,}qD-  D-� D-��D.z�D/  D/��D0  D0� D1D1�D2�D2� D3  D3� D4  D4� D5  D5}qD5�qD6}qD7  D7� D8�D8� D9�D9� D9�qD:}qD:�qD;}qD<�D<� D=  D=}qD>  D>}qD?  D?��D@  D@� DA  DA}qDA�qDB� DC  DC}qDC�qDD}qDD�qDE� DF  DF}qDG  DG��DH�DH��DI�DI� DJ�DJ��DK  DK� DL  DL� DM  DM� DN  DN� DN�qDO� DP  DP� DQ  DQ}qDR�DR� DR�qDS}qDT  DT��DU  DU� DV�DV� DW  DW� DX  DX� DY  DY}qDY�qDZ}qD[  D[��D\�D\� D\�qD]� D]�qD^� D_  D_}qD`  D`��D`�qDa}qDb  Db� Dc  Dc��Dd  Dd}qDe  De� Df�Df� Dg  Dg��Dh  Dh� Di�Di��Dj  Dj� Dk�Dk��Dl  Dl� Dm  Dm��Dn  Dn}qDo  Do��Dp�Dp� Dp�qDq� Dr�Dr� Ds  Ds� Dt  Dt� Du  Du}qDv  Dv� Dw  Dw� Dx  Dx}qDx�qDy��Dz�Dz}qD{  D{� D|  D|� D}  D}� D~�D~}qD  D}qD��D�@ D�~�D�� D�  D�@ D�~�D�� D�HD�AHD�� D��HD���D�=qD�~�D���D���D�@ D��HD�� D���D�@ D��HD�� D���D�AHD�� D�� D�HD�@ D�~�D�� D�  D�@ D��HD�D�  D�@ D�� D�� D���D�>�D�~�D�� D�HD�B�D��HD���D���D�@ D�� D�� D�  D�>�D��HD��HD�  D�@ D�~�D�� D�  D�>�D�� D��HD�  D�@ D�~�D���D�  D�B�D��HD��HD�HD�AHD��HD��HD�  D�@ D�� D��HD��D�@ D�~�D�� D�HD�B�D�� D�� D���D�>�D�� D���D�  D�@ D�� D��HD��D�@ D�� D��HD�  D�=qD�~�D���D�  D�@ D�� D�� D�HD�AHD�� D�� D��qD�>�D�� D���D�  D�>�D�~�D�� D��qD�@ D�� D�� D���D�>�D��HD���D�  D�AHD�� D�� D�  D�AHD���D�� D�HD�AHD�~�D���D�  D�@ D�~�D���D�  D�@ D�~�D��HD��D�@ D�~�D�� D�  D�B�D�� D���D�  D�@ D�}qD���D�HD�>�D�}qD�� D�HD�AHD���D��HD�  D�AHD��HD��HD�  D�>�D��HD�� D���D�@ D���D��HD�HD�AHD�~�D��HD�  D�@ D��HD���D��qD�>�D�� D�� D�HD�AHD�� D�� D�  D�>�D�~�D�� D�  D�AHD�� D�� D�HD�@ D�~�D�� D�  D�>�D�� D��HD���D�>�D�~�D��HD�  D�@ D�� D��qD��qD�>�D��HD��HD�  D�@ D�� D���D�  D�@ D�� D�� D���D�@ D��HD��HD�HD�AHD��HD��HD�  D�@ D���D�� D���D�>�D�~�D�� D�HD�@ D�� D�� D�  D�@ D�~�D���D�  D�AHD��HD��HD�HD�@ D�~�D��HD�  D�@ D D��HD��D�AHDÀ Dþ�D�  D�@ DĀ Dľ�D���D�@ DŁHDž�D�  D�@ D�~�Dƾ�D�  D�@ D�~�D�� D�  D�>�D�~�D�� D��D�AHDɀ D��HD�HD�AHDʀ D��HD���D�@ Dˀ D˾�D�  D�AHD�~�D�� D�  D�@ D̀ D��HD�HD�@ D�}qDξ�D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�>�D�~�DѾ�D�  D�@ DҀ DҽqD�  D�AHDӁHD��HD�  D�@ DԁHD�� D���D�>�DՀ D��HD�  D�@ Dր D־�D�  D�@ D׀ D׾�D���D�>�D؀ D�� D�  D�AHDفHD��HD�HD�@ D�~�D�� D�  D�AHDہHD�� D���D�@ D܀ D�� D�  D�AHD݁HD�� D�HD�@ D�~�D�� D��D�AHD߀ D�� D�  D�>�D�~�D��HD��D�@ D�~�DᾸD���D�@ D₏D��HD���D�=qD� D�� D�  D�AHD�HD侸D��qD�>�D� D��HD��D�AHD�~�D�� D�HD�@ D� D�� D���D�B�D�c�?#�
?B�\?k�?�z�?�33?���?�@�@�@&ff@333@B�\@Q�@^�R@p��@�G�@�ff@�{@�
=@�p�@��@�{@�z�@�p�@��@˅@�@�(�@��
@���@�z�@�p�A�\A
=A�A\)A�
A�A�A ��A%�A(Q�A.{A1�A5A:�HA?\)AB�\AG�AL(�AP  AU�AY��A]p�Ab�\AfffAj�HAo\)As�
AxQ�A|��A�Q�A��\A��A�
=A�G�A��
A�A�Q�A��\A�z�A�
=A���A��A�A��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            ?�=q@   @B�\@�G�@�  @��R@޸R@��RA��A ��A,(�AAG�A`��A�  A�  A�Q�A�  A��A�  A�Q�A�Q�B (�BQ�B(�B  B   B(  B0  B8  B?�
BG�
BO�
BX  B`Q�Bh(�Bp  Bw�
B�{B�{B�{B�{B�  B��B��B�  B�  B�  B�{B�{B�  B��B�  B��B�  B�  B�  B�  B�  B�  B�{B�{B�  B�  B�{B�{B��B��B�  B�  C   C
=C  C��C  C	��C��C
=C  C  C
=C
=C  C��C��C��C�C!��C$  C%��C(  C*
=C,
=C.
=C0  C1��C3��C5��C7��C:  C<
=C>
=C@  CB  CD  CF  CH
=CJ{CL  CN  CP  CQ��CS��CU��CX  CZ  C\  C^  C`  Cb  Cd  Cf
=Ch  Cj
=Cl  Cm�Co��Cq��Ct  Cv{Cx{Cz
=C{��C}��C�  C�C�C�  C�  C�  C�  C�  C�C�C�  C�  C�C�  C���C�  C�C�C�C�C�  C�  C�C�C�C�  C�  C�  C�  C�C�
=C�
=C�
=C�C�  C�  C�  C�  C�  C���C�  C�C�  C�  C�C�  C���C�  C�  C�  C�  C�C�C�C�C�C�  C�  C�  C�  C�  C�C�C�C�C�  C�C�C�  C���C�  C�C�  C�  C�  C�  C�C�C���C���C�  C�C���C�  C�C���C�  C�C�  C���C�  C�  C�  C�C�  C���C�C�  C���C�  C�C���C���C�  C�  C���C�C�C�C�C�C�
=C�
=C�
=C�  C���C�  C�
=C�C�C�  C���C���C�  C�
=C�
=C�C�D   D � D�D� D  D}qD�qD}qD  D� D  D��D�qD� D�D��DD��D	  D	}qD	�qD
� DD��D  Dz�D�qD}qD�qD� D�D� D�qD� D�D� D�D��D�D��D�D��D  Dz�D  D� D  D}qD  D� D  D��D  D��D  D� D  D� D  D��D  D}qD�qD��D   D }qD ��D!� D"  D"� D"�qD#}qD$�D$��D%�D%� D&  D&� D'�D'�D(�D(� D)  D)}qD)��D*}qD+  D+��D,  D,}qD-  D-� D-��D.z�D/  D/��D0  D0� D1D1�D2�D2� D3  D3� D4  D4� D5  D5}qD5�qD6}qD7  D7� D8�D8� D9�D9� D9�qD:}qD:�qD;}qD<�D<� D=  D=}qD>  D>}qD?  D?��D@  D@� DA  DA}qDA�qDB� DC  DC}qDC�qDD}qDD�qDE� DF  DF}qDG  DG��DH�DH��DI�DI� DJ�DJ��DK  DK� DL  DL� DM  DM� DN  DN� DN�qDO� DP  DP� DQ  DQ}qDR�DR� DR�qDS}qDT  DT��DU  DU� DV�DV� DW  DW� DX  DX� DY  DY}qDY�qDZ}qD[  D[��D\�D\� D\�qD]� D]�qD^� D_  D_}qD`  D`��D`�qDa}qDb  Db� Dc  Dc��Dd  Dd}qDe  De� Df�Df� Dg  Dg��Dh  Dh� Di�Di��Dj  Dj� Dk�Dk��Dl  Dl� Dm  Dm��Dn  Dn}qDo  Do��Dp�Dp� Dp�qDq� Dr�Dr� Ds  Ds� Dt  Dt� Du  Du}qDv  Dv� Dw  Dw� Dx  Dx}qDx�qDy��Dz�Dz}qD{  D{� D|  D|� D}  D}� D~�D~}qD  D}qD��D�@ D�~�D�� D�  D�@ D�~�D�� D�HD�AHD�� D��HD���D�=qD�~�D���D���D�@ D��HD�� D���D�@ D��HD�� D���D�AHD�� D�� D�HD�@ D�~�D�� D�  D�@ D��HD�D�  D�@ D�� D�� D���D�>�D�~�D�� D�HD�B�D��HD���D���D�@ D�� D�� D�  D�>�D��HD��HD�  D�@ D�~�D�� D�  D�>�D�� D��HD�  D�@ D�~�D���D�  D�B�D��HD��HD�HD�AHD��HD��HD�  D�@ D�� D��HD��D�@ D�~�D�� D�HD�B�D�� D�� D���D�>�D�� D���D�  D�@ D�� D��HD��D�@ D�� D��HD�  D�=qD�~�D���D�  D�@ D�� D�� D�HD�AHD�� D�� D��qD�>�D�� D���D�  D�>�D�~�D�� D��qD�@ D�� D�� D���D�>�D��HD���D�  D�AHD�� D�� D�  D�AHD���D�� D�HD�AHD�~�D���D�  D�@ D�~�D���D�  D�@ D�~�D��HD��D�@ D�~�D�� D�  D�B�D�� D���D�  D�@ D�}qD���D�HD�>�D�}qD�� D�HD�AHD���D��HD�  D�AHD��HD��HD�  D�>�D��HD�� D���D�@ D���D��HD�HD�AHD�~�D��HD�  D�@ D��HD���D��qD�>�D�� D�� D�HD�AHD�� D�� D�  D�>�D�~�D�� D�  D�AHD�� D�� D�HD�@ D�~�D�� D�  D�>�D�� D��HD���D�>�D�~�D��HD�  D�@ D�� D��qD��qD�>�D��HD��HD�  D�@ D�� D���D�  D�@ D�� D�� D���D�@ D��HD��HD�HD�AHD��HD��HD�  D�@ D���D�� D���D�>�D�~�D�� D�HD�@ D�� D�� D�  D�@ D�~�D���D�  D�AHD��HD��HD�HD�@ D�~�D��HD�  D�@ D D��HD��D�AHDÀ Dþ�D�  D�@ DĀ Dľ�D���D�@ DŁHDž�D�  D�@ D�~�Dƾ�D�  D�@ D�~�D�� D�  D�>�D�~�D�� D��D�AHDɀ D��HD�HD�AHDʀ D��HD���D�@ Dˀ D˾�D�  D�AHD�~�D�� D�  D�@ D̀ D��HD�HD�@ D�}qDξ�D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�>�D�~�DѾ�D�  D�@ DҀ DҽqD�  D�AHDӁHD��HD�  D�@ DԁHD�� D���D�>�DՀ D��HD�  D�@ Dր D־�D�  D�@ D׀ D׾�D���D�>�D؀ D�� D�  D�AHDفHD��HD�HD�@ D�~�D�� D�  D�AHDہHD�� D���D�@ D܀ D�� D�  D�AHD݁HD�� D�HD�@ D�~�D�� D��D�AHD߀ D�� D�  D�>�D�~�D��HD��D�@ D�~�DᾸD���D�@ D₏D��HD���D�=qD� D�� D�  D�AHD�HD侸D��qD�>�D� D��HD��D�AHD�~�D�� D�HD�@ D� D�� D���D�B�G�O�?#�
?B�\?k�?�z�?�33?���?�@�@�@&ff@333@B�\@Q�@^�R@p��@�G�@�ff@�{@�
=@�p�@��@�{@�z�@�p�@��@˅@�@�(�@��
@���@�z�@�p�A�\A
=A�A\)A�
A�A�A ��A%�A(Q�A.{A1�A5A:�HA?\)AB�\AG�AL(�AP  AU�AY��A]p�Ab�\AfffAj�HAo\)As�
AxQ�A|��A�Q�A��\A��A�
=A�G�A��
A�A�Q�A��\A�z�A�
=A���A��A�A��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��A��A��mA��
A܋DA�
=A۩�A�bNA�-A���A���Aڣ�Aڏ\AڅA�x�A�t�A�l�A�hsA�bNA�`BA�^5A�VA�VA�S�A�O�A�I�A�G�A�?}A�;dA�33A�$�A��A�1A���A���A��A��yA��/AټjAٕ�A�XA�1A�AԲ-A�ZA�bA�ĜA�dZA�ȴAΑhA�VA̅A�S�Aˣ�A�jAɕ�A���A�{A�p�A�7LA�n�A¬A�r�A��A�JA�x�A� �A��mA�VA�-A�O�A��mA�hsA��9A�ĜA���A���A��A���A�33A���A���A��A��wA���A�1A�ƨA���A�M�A�A��-A�\)A���A�=qA��A�|�A�
=A�~�A��A�M�A�-A�A��!A��A}�A|�jAz��Au
=AsG�Ao��Ae�7Ab��A^v�AX1'AT�DARȴAQx�AP�ANJAKXAH�DAC33AA�A@=qA?�A>Q�A=�A<�HA;�hA8Q�A6n�A4(�A1�;A133A0��A/��A+��A*�/A*��A*�+A*VA*E�A*1'A)�
A)\)A(�+A(9XA'7LA&�jA&z�A&$�A%��A%x�A%K�A$�/A#�#A"z�A!�A v�A  �AƨA\)A
=A1'A�AQ�AO�A=qA  AdZA"�AVA��A�A��A�!A��AVAVA1'A�A��A��AO�A��A1'A�TA�A��AĜA~�A$�A�hAl�AC�A%A��A�uA�AƨA��A�PA�7A�PA�hA�PAx�AG�A1'A�FA�PAA�A �A1A�^A��A�PA�7A�A�Al�A;dA�A%A
�/A
�RA
��A
�+A	��A	�hA	oA�9A��AjA�wAC�A�A�A/AK�AO�A�A%A�HAffA�AA�+AM�A�TA`BA�9AXA ȴA �DA ffA ^5A ZA VA ZA ^5A ZA b@��@��@��^@��D@��@�
=@��T@�-@�1@�l�@��y@���@��@�ff@��@�v�@�\@�$�@�h@�@�@�/@�I�@�I�@�9X@�  @���@�@��@���@�r�@�(�@�|�@��`@�(�@�|�@�33@�n�@��@�Q�@�ƨ@�\@�x�@��@�Ĝ@�@�j@ߝ�@�t�@���@�5?@���@�O�@��@�Ĝ@�z�@�A�@��@��;@�l�@��H@ڟ�@��@��@ش9@��m@֟�@�=q@�/@���@ԓu@�A�@��
@�ƨ@Ӯ@�|�@�|�@�l�@�S�@�@җ�@��T@�`B@��@���@�A�@�|�@�+@�n�@�A�@�l�@��@ʸR@�ff@�$�@��T@ɺ^@ɡ�@ɑh@�x�@��@�z�@���@Ǖ�@�\)@�
=@ư!@�M�@�J@���@��m@�C�@���@�5?@��^@���@�j@�b@��;@�l�@�33@���@��T@�`B@��@�(�@�+@���@���@��-@���@�%@�r�@�1'@�1@��
@�t�@�33@��R@�{@��@�7L@��j@�Z@�(�@���@���@�|�@�\)@��@��y@���@�ff@�5?@�-@��@�{@��T@��h@��u@�A�@�(�@�1@���@��;@��;@�ƨ@���@��P@��P@���@���@�|�@��@�E�@�hs@���@�Ĝ@��9@�z�@��m@�33@��@���@��+@�V@��^@��@�/@�Ĝ@��@��F@�l�@�33@�
=@��y@��H@���@���@�V@��@���@���@��7@��7@��@�x�@�X@��D@�(�@��;@��
@���@��w@���@�\)@�"�@���@�n�@��@�{@�@��@���@�x�@�Ĝ@� �@���@��F@���@�+@���@���@�^5@�=q@�-@�@���@�bN@�  @��;@��@���@��!@�v�@�@�@���@��h@�hs@�?}@�&�@�&�@�%@��u@�9X@�(�@��;@�|�@�K�@��y@��R@���@�^5@��-@�hs@���@���@��`@���@�j@�(�@�1@��@��;@��w@��P@�dZ@��\@�hs@�%@��@�1'@��@��
@��
@���@��F@��P@�t�@�dZ@��@��H@���@��@���@��h@�hs@�7L@��@���@���@��9@�z�@��
@�@���@�-@��^@��h@�G�@���@��9@�r�@�j@�Q�@�1@�|�@��@�
=@��y@���@���@�~�@�^5@�M�@�=q@�$�@�@�@�/@���@��@���@���@��\@�V@�{@���@��@��@��T@��T@��T@��#@���@���@���@���@���@���@�@�@�@�@�@��^@���@�`B@��@�V@��@��u@��@\)@~�y@~�+@~E�@~@|�@|z�@|(�@{��@{��@{"�@{o@z�H@z��@z�!@z~�@y�@xĜ@wl�@w
=@v�@vE�@u@u�@t�@t��@tj@s��@s��@sC�@r��@qX@q7L@q�@p��@p��@p�@p  @n��@m?}@lZ@k�m@kS�@j��@jn�@i�#@ihs@h�9@g�w@f��@fV@f{@e�T@e��@e�-@e��@ep�@d��@d�@dz�@dZ@dI�@d(�@d�@d1@c�m@cƨ@c��@c33@b��@a7L@_�w@]�@\��@\�@\j@\(�@[�m@[�
@[�
@[�
@[t�@[C�@Z�@Z^5@Z-@Y�#@X��@W�w@VE�@Up�@U`B@U/@T�D@S�m@So@R��@RM�@Qhs@P �@Ol�@O;d@Nȴ@N�+@Nff@N$�@N@M��@M�-@M�h@M�@M?}@L�/@L��@LI�@L�@K�F@KC�@J��@I�@H��@H�u@H�@Hr�@HQ�@H �@G�;@G\)@F��@E��@E`B@D��@D9X@Cƨ@B�@B^5@B�@Ax�@@��@@ �@?�;@?;d@>��@>V@>5?@>@=@=O�@=V@<�/@<�@;@9�7@7l�@5��@4�@4�/@4�@4z�@49X@41@3�m@3��@3@2��@2n�@2J@1��@1&�@0�@01'@/�w@/�@/�P@/\)@/K�@/+@/�@.��@.�y@.ȴ@.$�@-�h@-?}@-V@,�@,j@,9X@,�@+�
@+��@+"�@*n�@*-@)��@)hs@)X@)G�@)G�@)G�@(�u@(1'@'�@&��@&�+@&@%��@%��@%��@%@%/@$�@$�@$�@$�@$�@$�@$j@$I�@$I�@$(�@$�@#ƨ@#��@#t�@#S�@"^5@"�@"�@"J@"J@!��@!��@!��@"J@"�@!�@ �@ bN@ A�@ b@�w@�P@|�@l�@K�@+@
=@��@��@�-@�-@��@�h@�h@�@�@�@��@��@��@z�@j@Z@�m@�@dZ@�!@x�@�9@r�@  @�@��@�w@�w@�@�P@|�@l�@l�@l�@\)@K�@+@�@�y@ȴ@��@��@�+@�+@�+@�+@�+@v�@v�@v�@v�@v�@v�@v�@v�@v�@v�@ff@E�@5?A��A��A��A��A��A��A��A��A��A�VA��A��TA��
A��/A��A��A��Aܕ�AܑhAܓuA܅A�z�A�E�A�
=A��A���A۸RA۶FAۡ�AۃA�v�A�ffA�VA�M�A�=qA�7LA�$�A�oA�A���A��TA��
A���Aڲ-AڬAک�Aڣ�Aڟ�Aڡ�Aڛ�Aڛ�Aڙ�AړuAړuAڏ\Aډ7AڋDAډ7AڅAڇ+Aڇ+AڃAڅA�~�A�|�A�~�A�z�A�z�A�z�A�t�A�x�A�x�A�t�A�v�A�r�A�r�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            A��A��A��mA��
A܋DA�
=A۩�A�bNA�-A���A���Aڣ�Aڏ\AڅA�x�A�t�A�l�A�hsA�bNA�`BA�^5A�VA�VA�S�A�O�A�I�A�G�A�?}A�;dA�33A�$�A��A�1A���A���A��A��yA��/AټjAٕ�A�XA�1A�AԲ-A�ZA�bA�ĜA�dZA�ȴAΑhA�VA̅A�S�Aˣ�A�jAɕ�A���A�{A�p�A�7LA�n�A¬A�r�A��A�JA�x�A� �A��mA�VA�-A�O�A��mA�hsA��9A�ĜA���A���A��A���A�33A���A���A��A��wA���A�1A�ƨA���A�M�A�A��-A�\)A���A�=qA��A�|�A�
=A�~�A��A�M�A�-A�A��!A��A}�A|�jAz��Au
=AsG�Ao��Ae�7Ab��A^v�AX1'AT�DARȴAQx�AP�ANJAKXAH�DAC33AA�A@=qA?�A>Q�A=�A<�HA;�hA8Q�A6n�A4(�A1�;A133A0��A/��A+��A*�/A*��A*�+A*VA*E�A*1'A)�
A)\)A(�+A(9XA'7LA&�jA&z�A&$�A%��A%x�A%K�A$�/A#�#A"z�A!�A v�A  �AƨA\)A
=A1'A�AQ�AO�A=qA  AdZA"�AVA��A�A��A�!A��AVAVA1'A�A��A��AO�A��A1'A�TA�A��AĜA~�A$�A�hAl�AC�A%A��A�uA�AƨA��A�PA�7A�PA�hA�PAx�AG�A1'A�FA�PAA�A �A1A�^A��A�PA�7A�A�Al�A;dA�A%A
�/A
�RA
��A
�+A	��A	�hA	oA�9A��AjA�wAC�A�A�A/AK�AO�A�A%A�HAffA�AA�+AM�A�TA`BA�9AXA ȴA �DA ffA ^5A ZA VA ZA ^5A ZA b@��@��@��^@��D@��@�
=@��T@�-@�1@�l�@��y@���@��@�ff@��@�v�@�\@�$�@�h@�@�@�/@�I�@�I�@�9X@�  @���@�@��@���@�r�@�(�@�|�@��`@�(�@�|�@�33@�n�@��@�Q�@�ƨ@�\@�x�@��@�Ĝ@�@�j@ߝ�@�t�@���@�5?@���@�O�@��@�Ĝ@�z�@�A�@��@��;@�l�@��H@ڟ�@��@��@ش9@��m@֟�@�=q@�/@���@ԓu@�A�@��
@�ƨ@Ӯ@�|�@�|�@�l�@�S�@�@җ�@��T@�`B@��@���@�A�@�|�@�+@�n�@�A�@�l�@��@ʸR@�ff@�$�@��T@ɺ^@ɡ�@ɑh@�x�@��@�z�@���@Ǖ�@�\)@�
=@ư!@�M�@�J@���@��m@�C�@���@�5?@��^@���@�j@�b@��;@�l�@�33@���@��T@�`B@��@�(�@�+@���@���@��-@���@�%@�r�@�1'@�1@��
@�t�@�33@��R@�{@��@�7L@��j@�Z@�(�@���@���@�|�@�\)@��@��y@���@�ff@�5?@�-@��@�{@��T@��h@��u@�A�@�(�@�1@���@��;@��;@�ƨ@���@��P@��P@���@���@�|�@��@�E�@�hs@���@�Ĝ@��9@�z�@��m@�33@��@���@��+@�V@��^@��@�/@�Ĝ@��@��F@�l�@�33@�
=@��y@��H@���@���@�V@��@���@���@��7@��7@��@�x�@�X@��D@�(�@��;@��
@���@��w@���@�\)@�"�@���@�n�@��@�{@�@��@���@�x�@�Ĝ@� �@���@��F@���@�+@���@���@�^5@�=q@�-@�@���@�bN@�  @��;@��@���@��!@�v�@�@�@���@��h@�hs@�?}@�&�@�&�@�%@��u@�9X@�(�@��;@�|�@�K�@��y@��R@���@�^5@��-@�hs@���@���@��`@���@�j@�(�@�1@��@��;@��w@��P@�dZ@��\@�hs@�%@��@�1'@��@��
@��
@���@��F@��P@�t�@�dZ@��@��H@���@��@���@��h@�hs@�7L@��@���@���@��9@�z�@��
@�@���@�-@��^@��h@�G�@���@��9@�r�@�j@�Q�@�1@�|�@��@�
=@��y@���@���@�~�@�^5@�M�@�=q@�$�@�@�@�/@���@��@���@���@��\@�V@�{@���@��@��@��T@��T@��T@��#@���@���@���@���@���@���@�@�@�@�@�@��^@���@�`B@��@�V@��@��u@��@\)@~�y@~�+@~E�@~@|�@|z�@|(�@{��@{��@{"�@{o@z�H@z��@z�!@z~�@y�@xĜ@wl�@w
=@v�@vE�@u@u�@t�@t��@tj@s��@s��@sC�@r��@qX@q7L@q�@p��@p��@p�@p  @n��@m?}@lZ@k�m@kS�@j��@jn�@i�#@ihs@h�9@g�w@f��@fV@f{@e�T@e��@e�-@e��@ep�@d��@d�@dz�@dZ@dI�@d(�@d�@d1@c�m@cƨ@c��@c33@b��@a7L@_�w@]�@\��@\�@\j@\(�@[�m@[�
@[�
@[�
@[t�@[C�@Z�@Z^5@Z-@Y�#@X��@W�w@VE�@Up�@U`B@U/@T�D@S�m@So@R��@RM�@Qhs@P �@Ol�@O;d@Nȴ@N�+@Nff@N$�@N@M��@M�-@M�h@M�@M?}@L�/@L��@LI�@L�@K�F@KC�@J��@I�@H��@H�u@H�@Hr�@HQ�@H �@G�;@G\)@F��@E��@E`B@D��@D9X@Cƨ@B�@B^5@B�@Ax�@@��@@ �@?�;@?;d@>��@>V@>5?@>@=@=O�@=V@<�/@<�@;@9�7@7l�@5��@4�@4�/@4�@4z�@49X@41@3�m@3��@3@2��@2n�@2J@1��@1&�@0�@01'@/�w@/�@/�P@/\)@/K�@/+@/�@.��@.�y@.ȴ@.$�@-�h@-?}@-V@,�@,j@,9X@,�@+�
@+��@+"�@*n�@*-@)��@)hs@)X@)G�@)G�@)G�@(�u@(1'@'�@&��@&�+@&@%��@%��@%��@%@%/@$�@$�@$�@$�@$�@$�@$j@$I�@$I�@$(�@$�@#ƨ@#��@#t�@#S�@"^5@"�@"�@"J@"J@!��@!��@!��@"J@"�@!�@ �@ bN@ A�@ b@�w@�P@|�@l�@K�@+@
=@��@��@�-@�-@��@�h@�h@�@�@�@��@��@��@z�@j@Z@�m@�@dZ@�!@x�@�9@r�@  @�@��@�w@�w@�@�P@|�@l�@l�@l�@\)@K�@+@�@�y@ȴ@��@��@�+@�+@�+@�+@�+@v�@v�@v�@v�@v�@v�@v�@v�@v�@v�@ff@E�G�O�A��A��A��A��A��A��A��A��A��A�VA��A��TA��
A��/A��A��A��Aܕ�AܑhAܓuA܅A�z�A�E�A�
=A��A���A۸RA۶FAۡ�AۃA�v�A�ffA�VA�M�A�=qA�7LA�$�A�oA�A���A��TA��
A���Aڲ-AڬAک�Aڣ�Aڟ�Aڡ�Aڛ�Aڛ�Aڙ�AړuAړuAڏ\Aډ7AڋDAډ7AڅAڇ+Aڇ+AڃAڅA�~�A�|�A�~�A�z�A�z�A�z�A�t�A�x�A�x�A�t�A�v�A�r�A�r�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
CaB
B�B
B�B
B[B
@OB
C�B
A B
@�B
A�B
AUB
A�B
@�B
@�B
@�B
@B
@OB
@OB
@B
@OB
@OB
@OB
@OB
@OB
@OB
@OB
@B
@B
?�B
?�B
?}B
>�B
>wB
=�B
=qB
=<B
=B
<�B
<6B
:�B
9�B
8B
4nB
0�B
7�B
4�B
5�B
H�B
O�B
T�B
a�B
gB
��B
�=B
�\B
��B
�&B
ݘB
��B
�B
��B�BB+kB4B<jBA�BE�BV�BsMB{JBu%Bn�Bi�Bb�Bk�Bt�B��B|B|�B�oB{Bz�B|PBo5Bi�Bc�B`BB^jB[�BXyBR�BI�B;�B�B
��B
�KB
ÖB
��B
�OB
��B
kB
Z�B
OvB
 �B
	�B
GB	��B	�B	�B	��B	�JB	u�B	h>B	T�B	F�B	=qB	9�B	9$B	4B	0�B	"�B	33B	,B	+�B	/�B	6zB	3hB	49B	8RB	EmB	D�B	Z�B	\�B	\�B	\)B	`BB	��B	�GB	��B	��B	��B	�+B	��B	��B	��B	�B	�VB	��B	��B	�9B	�B	��B	ɆB	��B	��B	ӏB	�B	��B	��B	�ZB	��B	��B	�>B	��B	��B	��B
 �B	�"B
B
	B
�B
�B
$B
eB
7B
�B
"4B
(�B
33B
7LB
8�B
9$B
;0B
=B
>�B
A�B
C�B
D�B
E�B
GB
GB
D�B
D�B
C�B
B[B
@B
>�B
7�B
6FB
6FB
7�B
8�B
8�B
8�B
8�B
8�B
7�B
:�B
9$B
5B
2�B
1�B
/OB
/�B
0�B
33B
4B
5�B
6�B
8�B
9�B
:�B
9�B
9�B
:�B
;0B
:�B
;dB
<6B
7B
7B
2aB
0!B
.�B
.IB
,�B
+�B
+�B
.�B
6�B
9�B
@�B
@�B
?�B
>�B
=qB
:�B
5�B
0�B
/�B
,qB
)*B
%zB
OB
�B
�B
�B
MB
MB
MB
B
�B
MB
MB
B
�B
VB
DB
fB
+B
uB
 �B	��B	��B	��B	��B	�"B	�PB	�PB	��B
B
�B	��B
 �B
1B
VB
�B
�B
�B
�B
.B
bB
\B
oB
B
�B
�B
PB

	B
	�B
�B
	7B

�B
�B
	lB
	�B
	7B
	7B
	B
fB
	7B
	7B
fB
�B
_B
1B
�B
�B
�B
�B
YB
�B
%B
%B
SB
SB
�B
�B
%B
�B
�B
�B
DB
	7B
	lB
	�B
	7B
�B
	B
�B
1B
�B
�B
�B
1B
+B
+B
�B
YB
YB
�B
�B
�B
�B
�B
SB
B
SB
�B
SB
SB
SB
B
�B
�B
SB
�B
�B
�B
SB
SB
SB
�B
�B
%B
%B
YB
YB
YB
1B
1B
�B
�B
�B
_B
_B
�B
+B
_B
�B
	B
	B

	B
	�B
	lB
xB
xB

�B
B
xB
xB
B
�B
B
�B
xB
B
B
B
B
~B
�B
�B
~B
�B
PB
�B
�B
B
�B
�B
�B
�B
�B
\B
\B
�B
\B
�B
�B
�B
�B
�B
�B
VB
"B
VB
�B
�B
�B
bB
.B
.B
bB
hB
oB
:B
B
�B
oB
�B
:B
oB
�B
uB
@B
�B
B
�B
�B
uB
�B
FB
�B
B
�B
�B
�B
{B
�B
FB
B
�B
SB
�B
�B
�B
�B
�B
$B
$B
YB
�B
�B
+B
+B
_B
+B
�B
7B
�B
�B
�B
B
=B
	B
�B
	B
kB
B
7B
kB
IB
IB
CB
�B
�B
IB
�B
!B
�B
!B
�B
VB
�B
�B
!B
VB
�B
!B
!B
�B
�B
 �B
 �B
 �B
 �B
 �B
 �B
!-B
!-B
 �B
 �B
!-B
 �B
!-B
!-B
!�B
!-B
!-B
!bB
 �B
#:B
$�B
%B
%zB
&�B
&�B
%�B
&B
&LB
&B
&�B
&LB
%�B
&�B
&�B
&�B
(�B
(�B
)�B
)�B
)�B
*eB
*�B
)�B
*�B
*eB
,=B
,�B
-B
-�B
-CB
-wB
.�B
.�B
.�B
/B
/B
.�B
/OB
0�B
0�B
0UB
0�B
0�B
0�B
1'B
0�B
1'B
0�B
0�B
0�B
1[B
1�B
1[B
2�B
49B
5B
5tB
5B
5�B
5tB
5�B
5?B
5�B
5?B
5tB
5?B
5?B
5B
5B
5B
5?B
4�B
5B
4�B
5B
4�B
4�B
4�B
4�B
5B
5B
4�B
6FB
5�B
7�B
7�B
7LB
7�B
8RB
8�B
9�B
9�B
9�B
9�B
:�B
:�B
:�B
:�B
:�B
:�B
:�B
;dB
<6B
=B
<�B
<�B
=qB
=qB
>BB
>BB
>B
>BB
>BB
>�B
>wB
?HB
@B
?�B
?�B
?�B
?�B
?}B
?�B
A B
B'B
B[B
B�B
B�B
B�B
C�B
C-B
C�B
D3B
E9B
E�B
E�B
E�B
FB
FB
FB
E�B
FB
F�B
F�B
F�B
F�B
F�B
GB
F�B
GB
F�B
F�B
F�B
F�B
F�B
HB
J#B
K^B
K^B
K^B
K^B
K�B
K�B
K�B
K�B
K�B
L0B
K�B
L0B
L�B
K�B
L�B
MB
N<B
OBB
O�B
O�B
OBB
O�B
PHB
P�B
P�B
P�B
Q�B
R B
RTB
R�B
S[B
S�B
S�B
T,B
S�B
TaB
T,B
T,B
T,B
TaB
T�B
T�B
T�B
U2B
U2B
U�B
U�B
V�B
W?B
W
B
W?B
W
B
V�B
V�B
W
B
W
B
WsB
W
B
V�B
WsB
W�B
XEB
YKB
YKB
Y�B
ZB
Z�B
[�B
[�B
\]B
\�B
\�B
]/B
\�B
\�B
]�B
]dB
]/B
^B
^5B
_�B
`�B
cTB
c B
b�B
b�B
b�B
b�B
b�B
b�B
cTB
c�B
c�B
c�B
d�B
e,B
e�B
ffB
f�B
gmB
gmB
gmB
h
B
h
B
h�B
h>B
h�B
h�B
h�B
iyB
jB
jB
jB
jKB
kB
kB
j�B
kQB
kB
k�B
l�B
l�B
m)B
m]B
m]B
m�B
m]B
l�B
n/B
m�B
n�B
o B
o�B
pB
p;B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
poB
p�B
poB
qvB
qAB
qvB
qvB
qvB
qAB
q�B
q�B
q�B
sMB
r�B
r�B
sB
sB
r�B
r�B
r�B
r�B
rGB
sMB
s�B
tTB
tTB
t�B
t�B
t�B
t�B
t�B
u%B
t�B
u%B
u�B
v+B
v�B
v`B
v`B
v�B
v�B
v+B
v`B
v+B
wfB
wfB
w2B
wfB
w2B
v�B
w�B
w2B
v�B
x8B
yrB
y�B
y�B
zxB
zDB
z�B
z�B
zxB
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
{B
{JB
{B
{JB
{JB
{B
{JB
{JB
{B
{JB
{B
{�B
{�B
{B
{B
{B
{B
{B
{�B
{B
{�B
{�B
{�B
D3B
C�B
CaB
B[B
C�B
B�B
A�B
B�B
A�B
C�B
C-B
@�B
>B
A B
1[B
@�B
F�B
D�B
A B
>wB
;0B
?HB
D�B
@B
>wB
?�B
GEB
<6B
@�B
C-B
?�B
@�B
B'B
A�B
>B
A�B
@OB
A�B
@�B
@�B
A�B
A�B
@OB
B�B
@�B
?�B
A B
A B
?}B
AUB
@B
@B
AUB
@�B
@�B
A B
?�B
@OB
AUB
?�B
@OB
A B
?�B
@�B
@�B
?�B
@�B
@�B
?B
@�B
?�B
@B
@�B
?�B
@�B
@OG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            B
C{B
CDB
B�B
DmB
E�B
G�B
D>B
COB
C�B
CeB
C-B
AB
@�B
@�B
@=B
@�B
@qB
@EB
@cB
@gB
@�B
@XB
@dB
@qB
@yB
@3B
@QB
@	B
?�B
?�B
?B
>�B
>B
=�B
=iB
=CB
<�B
= B
;�B
;uB
:�B
;�B
B�B
?/B
6�B
=eB
P B
S�B
[�B
d B
l�B
��B
�B
�<B
�B
ՐB
�7B
�B
��B�B B#B.zB6�B>�BC{BJ�BbCBz�B~�BwBrVBo�Bq�B�(B�B�BWB��B��B��B��B�Bv�Bm�Be�Ba�B`uB^B[�B]BY�Bf�B2�B	B
�yB
��B
��B
��B
��B
u?B
m�B
o
B
(2B
�B
�B

aB	�B	ֻB	��B	�[B	��B	~�B	b}B	M�B	B�B	?�B	A]B	>�B	<mB	6{B	:�B	/�B	.�B	4iB	8qB	7�B	:"B	D�B	MUB	N�B	b�B	_�B	_<B	a`B	n�B	�pB	�%B	�B	�KB	��B	��B	�7B	��B	��B	�]B	��B	� B	ìB	�uB	�YB	�B	�NB	ʱB	�yB	�cB	�B	��B	��B	��B	�FB	��B	�TB	��B	�)B
 DB
|B	�YB
$B

B
B
�B
]B
�B
�B
B
#B
)"B
3�B
80B
9'B
:B
<FB
>�B
AB
CEB
G#B
E%B
E�B
HB
HdB
F�B
EdB
D=B
C�B
EB
ByB
:=B
6�B
6�B
8CB
8�B
8�B
8�B
8�B
92B
9MB
>�B
;"B
6B
5jB
3�B
0�B
0LB
24B
3�B
4mB
5�B
6�B
9B
:uB
;�B
:dB
:VB
;wB
;�B
:�B
<�B
>nB
9B
93B
3�B
0�B
0B
1B
.�B
,sB
+�B
.�B
69B
9�B
A�B
@�B
@�B
@�B
?�B
>�B
7�B
2)B
1�B
.�B
,�B
*�B
 �B
B
$B
�B
fB
aB
CB
B
�B
�B
�B
�B
QB
�B
�B
�B

zB

 B
�B	�>B	��B	��B	�!B	�MB	�&B	�eB	��B
�B
�B	��B
 �B
	�B
)B
�B
�B
PB
OB
�B
�B
�B
<B
�B
9B
ZB
B
�B

�B

�B
,B
�B

lB
.B
<B

tB
	�B
	XB
	7B

�B
	�B
	�B

tB
oB
	LB
�B
�B
�B
LB
�B
eB
=B
ZB
*B
B
sB
�B
;B

�B
�B

�B
B
	�B

<B

�B
	pB
�B
	gB
�B
hB
�B
�B
	 B
	�B
UB
�B
�B
�B
B
�B
1B

�B
	�B
�B
�B
�B
�B
B
�B
�B
uB
TB
�B
�B
%B
�B
�B
vB
�B
�B
�B
nB
�B
,B
�B
WB
.B
�B
�B
�B
�B
�B
4B
BB
�B
mB
IB
IB
	�B
	�B

B

�B

B

_B
gB
�B
'B
iB
B
�B
�B
�B
�B
.B
AB
�B
nB
hB
cB
B
�B
\B
�B
ZB
�B
�B
�B
EB
�B
�B
UB
�B
9B
�B
�B
�B
�B
�B
+B
	B
&B
�B
�B
eB
zB
�B
B
xB
�B
�B
bB
�B
�B
�B
+B
�B
GB
VB
�B
[B
�B
XB
�B
HB
�B
!B
iB
#B
�B
�B
CB
�B
aB
�B
IB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
VB
�B
�B
4B
4B
�B
YB
sB
�B
�B
DB
�B
KB
B
�B
�B
@B
?B
B
PB
�B
wB
FB
�B
B
�B
B
B
�B
�B
�B
�B
*B
aB
�B
�B
�B
�B
�B
 DB
 wB
aB
�B
 ZB
 wB
!�B
!fB
!CB
!uB
"PB
!|B
"B
!NB
 �B
!WB
!�B
!QB
!zB
!jB
!�B
!B
!�B
!�B
"�B
%�B
%�B
%�B
&�B
'B
&�B
%�B
&5B
&�B
&�B
&�B
&�B
&�B
'(B
'�B
(MB
)�B
)5B
*1B
*BB
*!B
*�B
*�B
*�B
+�B
,$B
.2B
-�B
.B
.�B
-�B
.NB
/�B
/B
/|B
/EB
/uB
/�B
0�B
1kB
0�B
0�B
1"B
0�B
0�B
1zB
1#B
1\B
1>B
1`B
1[B
2�B
20B
2lB
4�B
6DB
6B
6B
5�B
6B
5�B
5�B
5XB
5�B
5KB
5�B
5gB
5AB
4�B
5B
5#B
5EB
4�B
5B
4�B
5B
4�B
4�B
58B
5>B
5�B
5[B
5�B
6�B
6�B
8�B
8B
7�B
8B
8�B
9�B
:XB
9�B
:DB
:B
;GB
:�B
;;B
:�B
:�B
:�B
;�B
<�B
=�B
=�B
=4B
=�B
>&B
>BB
>�B
>gB
>eB
>�B
>�B
?1B
?uB
@�B
@QB
?�B
@<B
?�B
?�B
@SB
AGB
CHB
CHB
CB
CPB
C�B
C�B
DWB
C�B
D�B
E{B
F}B
FB
F/B
FB
F)B
F:B
F/B
F-B
F�B
GB
F�B
G	B
F�B
G
B
G,B
F�B
G?B
GB
F�B
GSB
G�B
H�B
JXB
MB
K�B
K�B
K�B
K�B
LB
LB
K�B
K�B
LDB
L�B
LJB
L�B
M"B
L�B
M�B
N�B
PB
P:B
O�B
O�B
O�B
P�B
QB
QB
QB
Q�B
R�B
R�B
R�B
S'B
S�B
S�B
S�B
TNB
T+B
T�B
TNB
TCB
TlB
T�B
UB
UKB
U5B
U�B
U�B
VB
ViB
W�B
WB
WB
WRB
W.B
W
B
WB
W�B
W�B
X(B
W�B
W�B
W�B
X!B
Y	B
Y�B
Y�B
ZLB
Z�B
[�B
\
B
\[B
\�B
]<B
]!B
]`B
]>B
]gB
]�B
]�B
^B
_WB
_�B
bB
b�B
dB
cAB
b�B
c*B
c8B
b�B
cB
cVB
c�B
dEB
dVB
diB
eB
e�B
f{B
f�B
gKB
g�B
g�B
g�B
h"B
h2B
h�B
hiB
h�B
h�B
i]B
j B
jwB
j�B
j�B
j�B
kXB
kJB
k<B
k�B
k�B
l�B
mB
m3B
m�B
muB
msB
m�B
myB
m�B
n�B
nnB
o�B
o�B
p)B
p=B
p?B
o�B
o�B
psB
qB
p�B
p�B
p�B
pwB
p�B
p�B
q�B
qJB
q�B
q�B
q�B
qyB
q�B
rB
r�B
s�B
r�B
r�B
s B
s.B
r�B
r�B
r�B
r�B
r�B
t�B
t"B
t�B
t�B
t�B
t�B
uB
t�B
u B
uUB
u4B
u�B
v�B
v[B
v�B
vyB
vwB
v�B
v�B
v7B
v}B
v�B
w�B
wrB
w^B
w�B
w`B
w�B
xEB
w�B
xB
y�B
zYB
zB
zdB
z�B
zpB
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
{B
{B
{XB
{wB
{DB
{bB
{aB
{�B
{NB
{OB
{�B
{`B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�G�O�B
D3B
C�B
CaB
B[B
C�B
B�B
A�B
B�B
A�B
C�B
C-B
@�B
>B
A B
1[B
@�B
F�B
D�B
A B
>wB
;0B
?HB
D�B
@B
>wB
?�B
GEB
<6B
@�B
C-B
?�B
@�B
B'B
A�B
>B
A�B
@OB
A�B
@�B
@�B
A�B
A�B
@OB
B�B
@�B
?�B
A B
A B
?}B
AUB
@B
@B
AUB
@�B
@�B
A B
?�B
@OB
AUB
?�B
@OB
A B
?�B
@�B
@�B
?�B
@�B
@�B
?B
@�B
?�B
@B
@�B
?�B
@�B
@OG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<��n<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<Eo^<#�
<#�
<#�
<#�
<#�
<�b�<��<��I<#�
<#�
<#�
<#�
<#�
<�-`<Zq<#�
<#�
<#�
<#�
<#�
<#�
<#�
<2W�<��z=,s�<���<��X<A�Y<D2�<@�<��<|F<2WF<�z<���<#�
<#�
<#�
<��<#�
<��=�n<Ny�<�<<���<gO�<#�
<#�
<#�
<#�
<:`_<Hª<�sY<#�
<#�
<#�
<#�
<#�
<#�
<#�
<Q��<#�
<+��<#�
<#�
<#�
<#�
<r��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al., 2007, JAOT;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   CTM: alpha=0.141C, tau=6.89s with error equal to the adjustment;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                   SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    202204261606322022042616063220220426160632202204261606322022042616063220220426160632SI  SI  ARFMARFM                                                                                                                                                2021073005343920210730053439IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021080907010020210809070100QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021080907010020210809070100QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               SI  SI  ARFMARFM                                                                                                                                                2022042213364820220422133648IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV2.1V2.1                                                                                                                                2022042616064520220426160645IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V1.1V1.1CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2022042616064520220426160645IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2022042616064520220426160645IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                